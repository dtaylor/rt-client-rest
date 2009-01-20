#!/usr/bin/perl
#
# This script tests whether timeout actually works.

use strict;
use warnings;

use Test::More;

use Error qw(:try);
use IO::Socket;
use RT::Client::REST;

my $server = IO::Socket::INET->new(
    Type => SOCK_STREAM,
    Reuse => 1,
    Listen => 10,
) or die "Could not set up TCP server: $@";

my $port = $server->sockport;

my $pid = fork;
if ($pid > 0) {
    plan tests => 8;
    for my $timeout (1, 2, 5, 10) {
        my $rt = RT::Client::REST->new(
            server => "http://localhost:$port",
            timeout => $timeout,
        );
        my $t1 = time;
        my ($e, $t2);
        try {
            $rt->login(qw(username a password b));
        } catch Exception::Class::Base with {
            $t2 = time;
            $e = shift;
        };

        isa_ok($e, 'RT::Client::REST::RequestTimedOutException');
        ok($t2 - $t1 >= $timeout, "Timed out after $timeout seconds");
    }
} elsif (defined($pid)) {
    my $buf;
    my $client = $server->accept;
    1 while ($client->read($buf, 1024));
} else {
    die "Could not fork: $!";
}

# vim:ft=perl:
