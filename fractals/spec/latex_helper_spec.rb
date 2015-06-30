require 'spec_helper'

require 'latex_helper'
RSpec.describe LatexHelper do
  describe 'graphics' do
    before(:each) do
      @helper = LatexHelper.new
    end

    it 'can extract all files in a directory' do
      rv = @helper.emit_latex_include_graphics_for('spec/samples/gfx', 'foo/bar/')
      expect(rv.size).to eq(12)
      expect(rv.first).to eq('\includegraphics[width=0.8\\linewidth]{{foo/bar/spec/samples/gfx/gnu_gcc_gcc_config_cris_cris_c_naive_time_series}.png}')
      expect(rv.last).to eq('\includegraphics[width=0.8\\linewidth]{{foo/bar/spec/samples/gfx/gnu_gcc_gcc_cp_decl2_c_tab3_log_log}.png}')

    end

    it 'can build tables' do
      rv = @helper.latex_data_table_for('spec/samples/analyses')
      expect(rv.size).to eq(8)
      expect(rv.first['h_value']).to eq('0.61891')
    end
    it 'can emit latex table row' do
      rv = @helper.emit_latex_table2_for('spec/samples/analyses')
      expect(rv.size).to eq(11)
      expect(rv.first).to eq('\\begin{longtable}{l l r r r}')
      expect(rv[1]).to eq('\\textbf{Filename} & \\textbf{Type} & \\textbf{H} & \\textbf{+/-} & \\textbf{R} \\\\')
      expect(rv[2]).to eq('{spec/samples/analyses/usr.sbin/bgpd/rde.c} & naive & 0.61891 & 0.0097795 & 0.99927 \\\\')
      expect(rv.last).to eq('\\end{longtable}')
    end

    it 'can calculate variance' do
      rv = @helper.variance_data_for('spec/samples/analyses')
      expect(rv.size).to eq(2)

      expect(rv['spec/samples/analyses/usr.sbin/bgpd/rde.c']['h_value_variance']).to eq(0.16517000000000004)
      expect(rv['spec/samples/analyses/usr.sbin/bgpd/rde.c']['r_value_variance']).to eq(0.0005699999999999594)
      expect(rv['spec/samples/analyses/usr.sbin/bgpd/rde.c']['confidence_interval_variance']).to eq(0.006276500000000001)
    end
  end
end
