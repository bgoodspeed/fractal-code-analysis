# Example usage:
# ruby time_series_extractor.rb time_series_extractor.rb > time_series.text

class TimeSeriesExtractor

  def extract(lines, config)

    lines.collect do |line|
      if config.has_key?(:trim)
        length = line.strip.length
      elsif config.has_key?(:tabsize)
        length = line.length

        extra = line.count("\t") * (config[:tabsize] - 1)
        length += extra
      else
        length = line.length
      end
      length

    end
  end
  def extract_time_series_from_file(filepath, config={})

    lines = IO.readlines(filepath)
    extract(lines, config)
  end

  def extract_time_series_from_dir(directory_name, exclusion_patterns=[])
    rv = {}
    Dir["#{directory_name}/**/*"].each do |filename|
      next if Dir.exists?(filename)
      next if !exclusion_patterns.empty? and (filename =~ Regexp.new("#{exclusion_patterns.join('|')}"))
      rv[filename] = extract_time_series_from_file(filename)
    end
    rv
  end

  def format_for_consumption(raw)
    raw.each_with_index.collect { |line_length, idx| "#{idx + 1}, #{line_length}" }
  end

  def emit(raw, iostream=$stdout)
    format_for_consumption(raw).each {|line| iostream.puts line }
  end

  def emit_file(filepath, iostream=$stdout, config={})
    emit(extract_time_series_from_file(filepath, config), iostream)
  end
end

if $0 == __FILE__

  raise "usage: #{$0} file_to_process" unless ARGV.length > 0
  tse = TimeSeriesExtractor.new
  tse.emit_file(ARGV.first)
end