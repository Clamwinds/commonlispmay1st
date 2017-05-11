def two_or_more(a,b,*c)
  puts "I require two or more arguments!"
  puts "And sure enough, I got: "
  p a, b, c
end

def default_args(a,b,c=1)
  puts "Values of variables: ",a,b,c
end

default_args(3,2)

STDIN.each {|line | p line }
STDIN.select{|line| line =~ /\A[A-Z]/ }
STDIN.map {|line | line.reverse }
/*This isn't working 5-10-2017


