

if $0 == __FILE__
  $: << 'lib'
  require 'time_series_extractor'
  raise "usage: #{$0} file_to_process" unless ARGV.length > 0

  paths_to_process = IO.readlines ARGV.first

  tse = TimeSeriesExtractor.new

  def process(tse, fn, path, f, label, base, nametag, conf)
    puts "processing #{fn}"

    tse.emit_file(path, f, conf)
    `./analyze.m #{fn} "#{label}"`

    renames = ['time_series.png', 'log_log.png', 'analysis.txt']
    renames.each {|to_rename| File.rename(to_rename, base + '_' + nametag + '_' + to_rename ) }

  end

  paths_to_process.each do |paths|
    path = paths.strip
    base = path.gsub('/','_').gsub('_Users_bg_src_openbsd_src_._', '').gsub('.c', '_c')


    label = base.gsub('_', ' ')

    fn = base + '_raw_data.txt'
    fn_trim = base + '_trimmed_raw_data.txt'
    fn_tab3 = base + '_tab3_raw_data.txt'
    fn_tab8 = base + '_tab8_raw_data.txt'
    f = File.new(fn, 'w')
    ft = File.new(fn_trim, 'w')
    ft3 = File.new(fn_tab3, 'w')
    ft8 = File.new(fn_tab8, 'w')

    begin 
      process(tse, fn, path, f, label + ' naive', base, 'naive', {})
      process(tse, fn_trim, path, ft, label + ' trim', base, 'trim', {:trim => true})
      process(tse, fn_tab3, path, ft3, label + ' tab3', base, 'tab3', {:tabsize => 3})
      process(tse, fn_tab8, path, ft8, label + ' tab8', base, 'tab8', {:tabsize => 8})
    rescue Exception => e
      puts "Got error: #{e}, continuing"
    end

  end

end
