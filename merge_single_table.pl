#!/usr/bin/env perl

use strict;
use warnings;

use File::Basename;

my $usage = "usage: $0 sampleA.readcount sampleB.readcount ...\n\n";

unless (@ARGV) {
    die $usage;
}

my @rsem_files = @ARGV;

unless (scalar @rsem_files > 1) {
    die $usage;
}

main: {

    my @name=@rsem_files;
   for(my $i=0;$i<@rsem_files;$i++){
        $name[$i]=basename($name[$i]);
        $name[$i]=~s/.count$//;
#       $name[$i]=~s/.Readcount_FPKM.xls$//;

  }
    my %data;

    foreach my $file (@rsem_files) {

        open (my $fh, $file) or die "Error, cannot open file $file";
   #    <$fh>;
        while (<$fh>) {
            chomp;
            last if(/no_feature/);
            my ($acc,$count,) = split(/\t/);
            $data{$acc}->{$file} = $count;
        }
        close $fh;
    }

    my @filenames = @rsem_files;
    foreach my $file (@filenames) {
        $file = basename($file);
    }

    print "geneID";
    print join("\t", "", @name) . "\n";
    foreach my $acc (keys %data) {

        print "$acc";

        foreach my $file (@rsem_files) {

            my $count = $data{$acc}->{$file};
            unless (defined $count) {
                $count = "0";
            }

            print "\t$count";

        }

        print "\n";

    }

#    open ID, ">GeneIDList";
#    print ID "$_\n" foreach (sort keys %data);
#    close ID;


    exit(0);
}
