class LatexHelper

  def emit_latex_include_graphics_for(dir, preamble, filetype = 'png', sort_overrides = ['_time_series.png', '_log_log.png'])
    pattern = "#{dir}/*.#{filetype}"
    files = Dir.glob(pattern)
    sorted = files.sort { |a,b|
      aprime = a
      bprime = b

      sort_overrides.each do |so|
        aprime = aprime.gsub(so, '')
        bprime = bprime.gsub(so, '')
      end

      rv = 1
      if (aprime == bprime)
        rv = -1 if a.end_with?(sort_overrides.first)
      else
        rv = (aprime <=> bprime)
      end

      rv
    }
    sorted.collect do |f|

      "\\includegraphics[width=0.8\\linewidth]{{#{preamble}#{f.gsub('.png', '')}}.png}"
    end
  end

  def latex_data_table_for(dir)
    rv = []
    Dir.glob("#{dir}/*_analysis.txt").each do |file|
      token = '_c_'
      token = '.js' if file.include?('.js')
      token = '.rb' if file.include?('.rb')
      token = '.py' if file.include?('.py')
      token = '.html' if file.include?('.html')
      token = '.java' if file.include?('.java')

      suffix = token
      if file.include?('_cs')
        token = '_cs'
        suffix = '.cs'
      end

      if token == '_c_'
        suffix = '.c'
      end


      actual_name = file.split(token).first
      short_name = actual_name.gsub('data/','').gsub('other_lang_other_langs_', '').gsub('ngx_http_mp4_','').gsub('_','/') + suffix
      analysis_type = file.split('_')[-2]
      analysis = IO.readlines(file).first

      rv << {
          'filename' => file,
          'short_name' => short_name,
          'analysis_type' => analysis_type,
          'h_value' => analysis.split('+-').first.split(' = ').last.strip,
          'confidence_interval' => analysis.split('(').first.split('+-').last.strip,
          'r_value' => analysis.split('R=').last.strip
      }
    end

    rv
  end


  def emit_latex_table2_for(dir, sort_key='r_value')
    data = latex_data_table_for(dir)
    rv = []
    rv << '\\begin{longtable}{l l r r r}'
    rv << '\\textbf{Filename} & \\textbf{Type} & \\textbf{H} & \\textbf{+/-} & \\textbf{R} \\\\'



    sorted = data.sort {|a,b| b[sort_key] <=> a[sort_key]}

    sorted.each do |elem|
      line = "{#{elem['short_name']}} & #{elem['analysis_type']} & "
      line += "#{elem['h_value']} & #{elem['confidence_interval']} & #{elem['r_value']} \\\\"
      rv << line
    end
    rv << '\\end{longtable}'
    rv
  end

  def variance_data_for(dir)
    data = latex_data_table_for(dir)

    rv = {}
    data.each do |elem|
      rv[elem['short_name']] = {} unless rv.has_key?(elem['short_name'])
      rv[elem['short_name']]['h_values'] = [] unless  rv[elem['short_name']].has_key?('h_values')
      rv[elem['short_name']]['r_values'] = [] unless  rv[elem['short_name']].has_key?('r_values')
      rv[elem['short_name']]['confidence_intervals'] = [] unless  rv[elem['short_name']].has_key?('confidence_intervals')

      rv[elem['short_name']]['h_values'] << elem['h_value'].to_f
      rv[elem['short_name']]['r_values'] << elem['r_value'].to_f
      rv[elem['short_name']]['confidence_intervals'] << elem['confidence_interval'].to_f
    end

    rv.each do |name, values|

      rv[name]['h_value_variance'] = rv[name]['h_values'].max - rv[name]['h_values'].min
      rv[name]['r_value_variance'] = rv[name]['r_values'].max - rv[name]['r_values'].min
      rv[name]['confidence_interval_variance'] = rv[name]['confidence_intervals'].max - rv[name]['confidence_intervals'].min
    end


    rv
  end

  def emit_latex_variance_data_for(dir)
    data = variance_data_for(dir)
    rv = []
    rv << '\\begin{longtable}{l r r r}'
    rv << '\\textbf{Filename} &  \\textbf{H} & \\textbf{+/-} & \\textbf{R} \\\\'


    data.each do |k,v|
      line = "{#{k}} & #{v['h_value_variance']} & #{v['confidence_interval_variance']} & #{v['r_value_variance']} \\\\"
      rv << line
    end

    rv << '\\end{longtable}'
    rv

  end



  def emit_latex_variance_data2_for(dir)
    data = variance_data_for(dir)
    rv = []
    rv << '\\begin{longtable}{l r}'
    rv << '\\textbf{Filename} &  \\textbf{H} \\\\'


    data.each do |k,v|
      line = "{#{k}} & #{v['h_value_variance']}  \\\\"
      rv << line
    end

    rv << '\\end{longtable}'

    rv << '\\begin{longtable}{l r}'
    rv << '\\textbf{Filename} &  \\textbf{+/-} \\\\'


    data.each do |k,v|
      line = "{#{k}} & #{v['confidence_interval_variance']}  \\\\"
      rv << line
    end

    rv << '\\end{longtable}'
    rv << '\\begin{longtable}{l r}'

    rv << '\\textbf{Filename} &  \\textbf{R} \\\\'


    data.each do |k,v|
      line = "{#{k}} & #{v['r_value_variance']}  \\\\"
      rv << line
    end

    rv << '\\end{longtable}'
    rv

  end

end

if $0 == __FILE__

  raise "usage: #{$0} analyses|variance|graphics dir_to_process" unless ARGV.length > 1
  lh = LatexHelper.new
  if ARGV.first == 'variance'
    puts lh.emit_latex_variance_data2_for(ARGV.last)
  elsif ARGV.first == 'analyses'
    puts lh.emit_latex_table2_for(ARGV.last)
  else
    puts lh.emit_latex_include_graphics_for(ARGV.last, 'fractals/')
  end

end