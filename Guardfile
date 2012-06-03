ignore_paths 'ebin'

notification :growl

guard :shell do
  watch /(.*)\.(erl|coffee|dtl|hrl|src)$/ do |f|
    puts "changed..#{f.first}"
    n "#{f.first} has changed, running build", "Attention!"
    `make`
    n 'done', "Rebuild"
  end
end
