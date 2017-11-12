use File::Find;

my $mdrx = rx｢\[(  <-[ \] ]>+  )\]\(<!before <alpha>+ '://'> ( <-[ \) ]>+ )\.md(\#.*)?\)｣ ;

my @traverse = find(dir => ".", type => 'file', name => /.+\.md$/) ;

for @traverse -> $fp {
    my $dr = $fp.dirname ;
    $fp.slurp.match($mdrx,:g).map: { 
        my $mdpath = $dr ~ "/" ~ $_[1] ~ ".md";
        if (! $mdpath.IO.e) { 
            say "━━━━ missing " ~ $mdpath ~ " in " ~ $fp.Str
        } else {
            say "OK " ~ $mdpath ~ " in " ~ $fp.Str
        }
    }
}
