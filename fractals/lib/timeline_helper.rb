class TimelineEntry
  attr_accessor :author, :year, :title, :id
end

# author={{Department of Defense}},
#     year={1993},
#         title={A Guide

class TimelineHelper


  def raw_data_for(filename)
    lines = IO.readlines(filename)

    rv = []
    lines.each_with_index do |line, index|
      next unless ((index + 1) % 3) == 0

      tle = TimelineEntry.new
      tle.id = rv.size + 1
      tle.author = lines[index - 2].gsub('author=','').gsub('{','').gsub('}','') .gsub(',','').strip
      tle.year   = lines[index - 1].gsub('year=', '').gsub('{','').gsub('}','') .gsub(',','').strip.to_i
      tle.title   = line.gsub('title=', '').gsub('{','').gsub('}','') .gsub(',','').strip

      rv << tle

    end

    rv.sort {|a,b| a.year <=> b.year }
  end
  def emit_javascript_data_for(filename, raw_data=nil)
    data = raw_data.nil? ? raw_data_for(filename) : raw_data

    data.collect{|tle| "{id: #{tle.id}, content: '#{tle.author}, #{tle.title}', start: '#{tle.year}-01-01'}," }
  end
end


if $0 == __FILE__

  raise "usage: #{$0} timeline.txt" unless ARGV.length > 0
  th = TimelineHelper.new

  rv = th.emit_javascript_data_for(ARGV.first)
  rv.each {|line| puts line }

end
