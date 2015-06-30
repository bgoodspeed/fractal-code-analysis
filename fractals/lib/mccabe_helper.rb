module Enumerable
  def foldl(o, m = nil)
    inject(m) {|m, i| m ? m.send(o, i) : i}
  end
end

class MccabeHelper
  def raw_data_for(filename)
    pmc = `pmccabe #{filename}`
    lines = pmc.split("\n")
    output = lines.collect {|line| line.split(/\W+/)}
    output
  end

  def last_mccabe_count
      @mccabe_function_count
  end

  def mccabe_sum_for(filename)
    data = raw_data_for(filename)

    mccabe_values = data.collect {|line| line.first.to_i }
    @mccabe_function_count = data.size
    mccabe_values.foldl(:+, 0)
  end
end


if $0 == __FILE__
  raise "usage: #{$0} file_to_process" unless ARGV.length > 0

  paths_to_process = IO.readlines ARGV.first
  helper = MccabeHelper.new
  data = paths_to_process.collect do |paths|
    path = paths.strip
    mccabe = helper.mccabe_sum_for(path)
    [path, mccabe.to_f/helper.last_mccabe_count.to_f, mccabe]

  end

  sorted = data.sort {|a,b| b.last <=> a.last }
  rv = []
  rv << '\begin{tabular}{l r}'
  sorted.each do |filename, score|
    name = filename.gsub('/Users/bg/src/openbsd/src/./', '').gsub('_','-') #HACKHACKHACK stupid latex.
    rv << "{#{name}} & #{score} \\\\"
  end

  rv << '\end{tabular}'

  puts rv

  end
