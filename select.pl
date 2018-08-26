#!/usr/bin/perl -w
use strict;
use warnings;
use FindBin qw($Bin $Script);
use File::Basename qw(basename dirname);
use Getopt::Long;

my $usage="$0 [options]
-cls     <str> select the [ line|fa|onlyfa ] from geneid
-l       <str> geneid list
-s       <str> object
-n       <str> the column number of geneid, default 1
-o       <str> output

\nExample:
$0 -cls line -l geneid -s function -o select.geneid.function
$0 -cls fa -l geneid -s fasta -o select.geneid.fasta
\n";

my ($cls,$list,$object,$num,$out);
GetOptions("cls:s"=>\$cls,
               "l:s"=>\$list,
                   "s:s"=>\$object,
                   "n:s"=>\$num,
                   "o:s"=>\$out
                );
die $usage if !defined $cls;

# extract the line information

$num ||=1;

open IN2,$object||die;
open OUT,">$out"||die;
my %hash;
if($cls ne "onlyfa"){
        open IN1,$list||die;
    while(<IN1>){
        chomp;
        $hash{$_}=$_;
        }
}
if($cls eq "line"){
   while(<IN2>){
           chomp;
           my @info=split(/\t/);
       if(exists $hash{$info[$num -1]}){
                   print OUT "$_\n";
           }else{
                   next;
       }
   }
}

# extract the fasta sequence
if($cls eq "fa"){
        $/=">";<IN2>;
        while(<IN2>){
                next unless (my ($id,$seq) = /(.*?)\n(.*)/s);
               my $id2=(split(/\s/,$id))[0];
                $seq =~ s/[\d\s>]//g;
        if(exists $hash{$id2}){
                        print OUT ">$id2\n$seq\n";
                }
        }
        $/="\n";
}

# extract the only geneid and sequence from fasta
if($cls eq "onlyfa"){
          $/=">";<IN2>;
          while(<IN2>){
                  next unless (my($id,$seq)= /(.*?)\n(.*)/s);
          print OUT  &get_fasta($id,$seq);
          }
}
close IN1;
close IN2;
close OUT;

sub get_fasta{
        my $idinfo=shift;
        my $seqinfo=shift;
        my $id2=(split(/\s/,$idinfo))[0];
        $seqinfo=~s/[\d\s>]//g;
        return ">".$id2."\n".$seqinfo."\n";
}
