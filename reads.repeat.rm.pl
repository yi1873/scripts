#!/usr/bin/perl -w  
use strict;

# After reads fastqc, if there are several repeat sequence, we will remove the reads with repeat sequence;

my($in1,$in2,$out)=@ARGV;
open IN1,$in1||die;
open FASTQ,"gunzip -c $in2 |"||die;
open OUT,"|gzip -c >$out"||die;

my %hash;
while(<IN1>){
    chomp;
    my $repeat=(split(/\t/,$_))[0];
    my $kmer=substr($repeat,0,30);
    $hash{$kmer}=1;
}

while(my $readid = <FASTQ>) {
    chomp $readid;
    chomp (my $sequence  = <FASTQ>);
    chomp (my $comment   = <FASTQ>);
    chomp (my $quality   = <FASTQ>);
    my $num=length($sequence) - 30 + 1;
    my $count=0;
    for (my $i=0;$i<$num;$i++){
        my $kmer=substr($sequence,$i,30);
        if(exists $hash{$kmer}){
            $count += 1;
        }
    }
    if($count eq "0"){
        print OUT "$readid\n$sequence\n$comment\n$quality\n";
    }
}

close IN1;
close FASTQ;
close OUT;
