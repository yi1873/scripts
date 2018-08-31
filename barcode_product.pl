#!/usr/bin/perl -w

# 生成10套8nt的barcode，每套barcode尽可能多；

use strict;

my $in = shift;
open IN,$in||die;

my %barcode;
while(<IN>){
	chomp;
	my $seq = (split("\t",$_))[-1];
	$barcode{$seq}++;
}
for my $n(1..10){
	open OUT, ">barcode.$n.out";
	my %all=();
	my $count = 0;
	my %buf_code = %barcode;
	while ($count < 4**8){		
		my $seq = &get_random_seq;
		if(!exists $all{$seq}){
			$all{$seq}++;
	    $count++;
		  my $gc_count = $seq=~tr/GC/GC/;
	    next if $gc_count > 5 or $gc_count < 3;
		  next if $seq=~/AAA|TTT|CCC|GGG/;
	      if (&check_seq($seq,\%buf_code)){
		    	$buf_code{$seq}++;
		    	print OUT "$seq\n";
			  }
		}
	}
	#my @num=keys %all;
	#print @num."\n";last;
}
close IN;
close OUT;

sub get_random_seq{
	my @a = qw/A T C G/;
	my $s="";
	for (1..8){
		my $index = int(rand(3.999999));
		$s = $s.$a[$index];
	}
	return $s;
}

sub check_seq{
	my ($seq,$hash_ref) = @_;
	my $max_so_far = 0;
	my @ss = split(//,$seq);
	for my $barcode (keys %$hash_ref){
		my @rs = split(//,$barcode);
		my $same_nt_count = 0;
		#print STDERR "Compareing $seq\t $barcode";
		for my $ni(0..$#ss){
			if ($ss[$ni] eq $rs[$ni]){
				$same_nt_count++;
			}
		}
	    $max_so_far = $same_nt_count if $same_nt_count > $max_so_far;
	}
	if ($max_so_far>4){
		return 0;
	}else{
		return 1;
	}
}

