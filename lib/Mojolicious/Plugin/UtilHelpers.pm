package Mojolicious::Plugin::UtilHelpers;
use Mojo::Base 'Mojolicious::Plugin';

our $VERSION = '0.5_3';

use Encode ();
use Mojo::JSON;
use Mojo::ByteStream 'b';

require Data::Dumper;

has json => sub { Mojo::JSON->new };

sub register {
	my $self = shift;
	
	$self->common(@_);
	$self->http(@_);
	
	$self->util(@_);
	$self->text(@_);
	
	$self->format(@_);
	$self->route(@_);
}

sub common {
	my ($self, $app, $conf) = @_;
	
	$app->helper(db     => sub { shift->app->db });
	$app->helper(conf   => sub { shift->app->conf->{+shift} });
	$app->helper(vu     => sub { shift->tx->req->url->path->parts->[+shift] || '' });
	$app->helper(dumper => sub {
		shift;
		Data::Dumper->new([@_])->Indent(1)->Terse(1)->Maxdepth(3)->Dump;
	});
}

sub http {
	my ($self, $app, $conf) = @_;
	
	$app->helper(is_iphone => sub {
		my $ua = shift->tx->req->headers->user_agent;
		return $ua && $ua =~ /(cfnetwork|iphone|ipod|ipad)/i ? $1 : 0;
	});
	
	$app->helper(is_mobile_device => sub {
		return shift->req->headers->user_agent =~
			/(iPhone|iPod|iPad|Android|BlackBerry|Mobile|Palm)/
		? 1 : 0;
	});

	$app->helper(is_mobile => sub {
		my $self = shift;
		return $self->is_mobile_device && $self->is_iphone ne 'iPad' ? 1 : 0;
	});

	$app->helper(ip => sub {
		my $self = shift;
		my $for  = $self->req->headers->header('X-Forwarded-For');

		return
			$for && $for !~ /^192\.168\./ && $for !~ /unknown/i ? $for : undef # hack
		 ||
			$self->req->headers->header('X-Real-IP')
		 ||
			$self->tx->{remote_address}
		;
	});
	
	$app->helper(status => sub { my $self = shift;
		$self->res->code(shift || 200);
		$self->rendered;
	});

	$app->helper(redirect => sub { my $self = shift;
		my $url = shift || $self->return_url;
		Encode::_utf8_off($url) if Encode::is_utf8($url);
		
		$self->res->code(302);
		$self->res->headers->location( $url );
		$self->rendered;
	});
	
	$app->helper(return_url => sub { my $self = shift;
		my $default = shift || '/';
		my $referer = $self->req->headers->header('Referer');

		return $referer && $referer !~ /login|logout|enter/ ? $referer : $default; 
	});

	$app->helper(redirect_accel => sub { my $self = shift;
		my $url  = shift || return;
		my $type = shift || '';

		for ($self->res->headers) {
			$_->content_type( $type );
			$_->header( 'X-Accel-Redirect' => $url );
		}

		$self->rendered;
	});
	
	$app->helper(xhr_redirect => sub {
		my $self = shift;
		my $json = shift || {};
		my $url  = shift;
		
		return $self->req->is_xhr
			? $self->render_json( $json )
			: $self->redirect( $url )
		;
	});
	
	$app->helper(get_cookie => sub { my $self = shift;
		my $name = shift || '';
		my $cookie = $self->req->cookie( $name );
		return $cookie ? $cookie->value : '';
	});
}

sub util {
	my ($self, $app, $conf) = @_;
	
	$app->helper(check_error => sub {
		my $self  = shift;
		my $field = shift || return;
		
		return unless $self->stash('error');
		
		if (my $error = $self->stash('error')->{ $field }) {
			$self->render_partial('etc/validate') unless $self->stash('validate');
			return $self->stash('validate')->{ $error } || $error;
		}
	});
	
	$app->helper(list_splice => sub {
		my $self  = shift;
		my $list  = shift || return [];
		my $count = shift || 2;
		
		my $new; push @$new, [grep { $_ } splice @$list, 0, $count] while @$list;
		return $new;
	});
	
	my $helper = $self;
	$app->helper(perl2json => sub {
		my $self = shift;
		my $json = $helper->json->encode( @_ );
		return b( $json )->decode('utf8');
	});
	
	$app->helper(cat => sub {
		my $self = shift;
		my $tmpl = shift || return '';
		my $file = $self->conf('path')->{tmpl} . "/$tmpl.html.ep";
		
		my $data = do { local $/; open my $fh, '<:utf8', $file or return; <$fh> };
		return $data || '';
	});
	
	$app->helper(meta => sub {
		my $self = shift;
		my %meta = %{$self->conf('meta')}; # clone conf meta
		
		if (my $new = $self->stash('meta')) {
			$meta{$_} = $new->{$_} for keys %$new;
		}
		elsif (my $t = $self->stash('title')) {
			my @title = ref $t ? @$t : $t||();
			$meta{title} = join '. ', reverse @title if @title;
		}
		
		$_ = $self->text_fix($_) for $meta{description};
		
		$self->stash(meta => \%meta);
		return $self->stash('meta');
	});
	
	$app->helper(page_param => sub {
		my $self  = shift;
		my $page  = shift;
		my $name  = $self->stash('page_name') || 'page';
		
		my $param = $self->req->params;
		
		my $rm    = $self->stash('remove_param') || [];
		$param->remove($_) for @$rm;
		
		$param->remove($name);
		$param->append($name => $page) if $page;
		
		my $str = $param->to_string;
		return $self->url_for . ($str ? "?$str" : '');
	});
}

sub text {
	my ($self, $app, $conf) = @_;
	
	$app->helper(format_digital => sub {
		my $self = shift;
		my $d    = shift || return;
		my $sep  = shift || ' ';
		
		$d =~ s/(\d)(?=((\d{3})+)(\D|$))/$1$sep/g;
		return $d;
	});
	
	$app->helper(shorty => sub { my $self = shift;
		my $str    = shift || return;
		my $length = shift || 20;
		
		for ($str) {
			s/&nbsp;/ /sg;
			s/&amp;/&/sg;
			s/&quote?;/'/sg;
			s/&ndash;/–/sg;
			s/&mdash;/—/sg;
			s/&lquot;/«/sg;
			s/&rquot;/»/sg;
		}
		
		return length $str > $length ? substr($str, 0, $length) . '...' : $str;
	});
	
	$app->helper(shorty_fix => sub { my $self = shift;
		my $short = $self->shorty(@_) || return '';
		
		for my $tag (qw(strong span p div)) {
			my $start = @{[ $short =~ m{<$tag[^>]*>}g ]};
			my $end   = @{[ $short =~ m{</$tag>}g     ]};
			
			next unless my $c = $start - $end;
			
			$short .= join "\n", ( "</$tag>" ) x $c;
		}
		
		return $short;
	});
	
	$app->helper(link_title => sub { my $self = shift;
		my $link = shift || return;
		
		$link =~ m{http://(?:[\w\.]+\.)?(\w+)\.\w+};
		
		return $1 ? ucfirst $1 : '';
	});
	
	$app->helper(text_url => sub { my $self = shift;
		my $text  = shift || return;
		my $short = sub { my $t=shift; return length $t > 35 ? substr($t,0,35).'...' : $t; };
		
		$text =~ s{(http://\S+)}{qq(<a href="$1" class="external">) . $short->($1) . q(</a>)}seg;
		$text =~ s{\n+}{<br/>}g;
		
		return $text;
	});
	
	# XXX
	$app->helper(paragraph => sub { my $self = shift;
		my $info = shift || return;
		#$info =~ s\\\g;
		# XXX: Malformed UTF-8 character (unexpected continuation byte 0x98, with no preceding start byte 
		# eval { $info =~ s{(.*?)(\n\s){2,}}{<p>$1</p>\n}sg;  };
		
		$info = "<p>$info</p>" unless $info =~ /<p>/;
		return $info;
	});
}

sub format {
	my ($self, $app, $conf) = @_;
	
	# min support
	
	$app->types->type(min => 'text/html');
	$app->plugins->add_hook(after_static_dispatch => sub {
		my $c = shift;
		# warn('Min format ' . $c->req->url),
		$c->stash(format => 'min') if $c->req->param('min');
	});
}

sub route {
	my ($self, $app, $conf) = @_;
	
	$app->routes->add_shortcut(crud => sub {
		my($r, $name, $controller) = @_;
		
		unless ($controller) {
			$controller = $name;
			$controller =~ s{/}{-}g;
		}
		
		(my $rname = $controller) =~ s{-}{_}g;
		
		my $t = $r->route("/$name")->to("$controller#");
		
		$t->route->get ->to('#list')->name($rname);
		$t->route->post->to('#listing');
		
		$t->route('/sort')->get ->to('#sort')->name($rname.'_sort');
		$t->route('/sort')->post->to('#sorting')->name($rname.'_sort');
		
		$t->route('/add')->get ->to('#form', add => 1)->name($rname.'_add');
		$t->route('/add')->post->to('#add' );
		
		my $one = $t->bridge('/:id', id => qr/\d+/)->to('#check');
		$one->route->to('#item')->name($rname.'_item');
		$one->route('/edit')->get ->to('#form')->name($rname.'_edit');
		$one->route('/edit')->post->to('#edit');
		$one->route('/remove')->to('#remove')->name($rname.'_remove');
		
		$t->route('/:filter')->get->to('#list')->name($rname);
	});
}

1;

__END__

=head1 NAME

Mojolicious::Plugin::UtilHelpers - some util helpers for web development

=head1 SYNOPSIS

	# Mojolicious
	$self->plugin('util_helpers');
	
	# Mojolicious::Lite
	plugin 'util_helpers';

=head1 DESCRIPTION

L<Mojolicous::Plugin::UtilHelpers> is a plugin contains util helpers.

=head1 METHODS

L<Mojolicious::Plugin::UtilHelpers> inherits all methods from
L<Mojolicious::Plugin> and implements the following new ones.

=head2 C<register>

	$plugin->register;

Register plugin hooks in L<Mojolicious> application.

=head2 C<common>

Common helpers.

=over 3

=item * db

	$self->db

DB helper, fast access to the dbh of application.

=item * conf

	$self->conf('server')->{www}
	
	# tmpl
	%= conf('server')->{www}

Conf helper, fast access to the conf of application.

=item * vu

	$self->vu(0)
	
	# tmpl
	%= vu(0)

Parts of URL path or vitrual URI.

=back

=head2 C<http>

HTTP helpers.

=over 10

=item * is_iphone

	$self->is_iphone

Check User-Agent headers, returns true if ua =~ iPhone or iPod or iPad or CFNetworks.

=item * is_mobile_device

	$self->is_mobile_device

Check User-Agent headers, returns true if ua =~ iPhone or iPod or iPad or Android or BlackBerry or Palm or Mobile.

=item * is_mobile

	$self->is_mobile

Check User-Agent headers, same as C<is_mobile_device> without iPad.

=item * ip

	$self->ip

Helper returns ip of client, check X-Forwarded-For header, than X-Real-IP header and at last remote address of client.

=item * status

	$self->status(404)

Helper generates a response with any code without body.

=item * redirect

	$self->redirect( $url )

302 redirect, uses return_url helper unless $url.

=item * return_url

	$self->return_url( $default_url )

Helper can return previlous url (same as referer url, exclude login or logout or enter URL).
If referer url is empty, return_url returns $default_url or root url '/'.

=item * redirect_accel

	$self->redirect_accel( $url, $content_type )

Helper generates redirect with X-Accel-Redirect and Content-Type headers.

=item * xhr_redirect

	$self->xhr_redirect( { ok => 1 }, $redirect_url )

Helper renders JSON (if request has X-HTTP-Request header) or return redirect helper.

=item * get_cookie

	$self->get_cookie( $name )

Helper returns one value of cookie $name.

=back

=head2 C<util>

Util helpers.

=over 6

=item * check_error

	# tmpl
	%= check_error 'field_name'

Check stash error, if exists any errors, helper loads 'etc/validate' template and returns value of key 'field_name' in stash 'validate' hash.

=item * list_splice

	# tmpl
	% my $new = list_slice $list, $count;

Helper generates array of array (size is $count).

=item * perl2json

	$self->perl2json( $data )

Helper encodes perl structure to JSON.

=item * cat

	$self->cat('about/info')

Helper includes template file (format is html, handler is ep) without cache template.

=item * meta

	$self->meta

Helper uses meta from conf and stash 'meta' if exists and generate new stash 'meta'.

=item * page_param

	$self->page_param( $page )

Helper generates url, replace old page param.

=back

=head2 C<text>

Text and string helpers.

=over 6

=item * format_digital
	
	%= format_digital 123456
	
Helper returns 123 456.
Second parameters is separator, default value is space.

=item * shorty

	%= shorty $str, $length
	
Helper returns short string, $length is length of short string, default value is 20.

=item * shorty_fix

	%== shorty_fix $str, $length
	
Helper same as shorty, but tries fix bad tags.

=item * link_title

	%= link_title $link
	
Helper returns ucfirst name of domain.

=item * text_url

	%== text_url $text
	
Helper returns $text, where links are real href (<a href="$link" target="_blank">$link</a>).

=item * paragraph

	%== paragraph $info
	
Helper returns text with paragraph (<p />).

=back

=head2 C<format>

New formats for a Mojolicious application.

=over 1
=item * min

Type is text/html.
After static dispatch set stash 'format' as 'min' if request has min parameters.

Template name is C<name.min.ep>.

=back

=head2 C<route>

Add new shortcut to the routes of application.

=over 1
=item * crud

Create-read-update-delete routes for admin item.

	$r->crud(blog => 'admin-blog');
	
There are a lot of routes:

	GET  /blog -> admin-blog#list (name is admin_blog)
	POST /blog -> admin-blog#listing
	
	GET  /blog/sort -> admin-blog#sort (admin_blog_sort)
	POST /blog/sort -> admin-blog#sorting
	
	GET  /blog/add  -> admin-blog#form, stash 'add' => 1 (admin_blog_add)
	POST /blog/add  -> admin-blog#add
	
	bridge /blog/:id, id => qr/\d+/ -> admin-blog#check
	
	GET  /blog/:id -> admin-blog#item (admin_blog_item)
	
	GET  /blog/:id/edit -> admin-blog#form (admin_blog_edit)
	POST /blog/:id/edit -> admin-blog#edit
	
	GET  /blog/:id/remove -> admin-blog#remove (admin_blog_remove)
	
	GET  /blog/:id/:filter -> admin-blog#list (admin_blog)

=back

=head1 SEE ALSO

L<Mojolicious>, L<Mojolicious::Guides>, L<http://mojolicious.org>.

=head1 AUTHOR

Anatoly Sharifulin <sharifulin@gmail.com>

=head1 BUGS

Please report any bugs or feature requests to C<bug-mojolicious-plugin-UtilHelpers at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.htMail?Queue=Mojolicious-Plugin-UtilHelpers>.  We will be notified, and then you'll
automatically be notified of progress on your bug as we make changes.

=over 5

=item * Github

L<http://github.com/sharifulin/Mojolicious-Plugin-UtilHelpers/tree/master>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.htMail?Dist=Mojolicious-Plugin-UtilHelpers>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Mojolicious-Plugin-UtilHelpers>

=item * CPANTS: CPAN Testing Service

L<http://cpants.perl.org/dist/overview/Mojolicious-Plugin-UtilHelpers>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Mojolicious-Plugin-UtilHelpers>

=item * Search CPAN

L<http://search.cpan.org/dist/Mojolicious-Plugin-UtilHelpers>

=back

=head1 COPYRIGHT & LICENSE

Copyright (C) 2010-2011 by Anatoly Sharifulin.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut
