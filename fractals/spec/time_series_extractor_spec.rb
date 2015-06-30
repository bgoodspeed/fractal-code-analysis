require 'spec_helper'

require 'time_series_extractor'
RSpec.describe TimeSeriesExtractor do
  describe 'ruby' do
    before(:each) do
      @extractor = TimeSeriesExtractor.new
    end

    it 'can analyze a ruby file' do
      rv = @extractor.extract_time_series_from_file('spec/samples/ruby/simple_demo.rb')
      expect(rv.length).to eq(20)
      expect(rv.first).to eq(1)
      expect(rv.last).to eq(3)
    end
  end
  describe 'c' do
    before(:each) do
      @extractor = TimeSeriesExtractor.new
    end

    it 'can analyze a c file' do
      rv = @extractor.extract_time_series_from_file('spec/samples/c/main.c')
      expect(rv.length).to eq(8)
      expect(rv.first).to eq(19)
      expect(rv.last).to eq(2)
    end
  end
  describe 'dir' do
    before(:each) do
      @extractor = TimeSeriesExtractor.new
    end

    it 'can analyze a whole directory' do
      rv = @extractor.extract_time_series_from_dir('spec/samples/dir')
      expect(rv.keys.size).to eq(3)
      expect(rv).to have_key('spec/samples/dir/another.c')
      expect(rv).to have_key('spec/samples/dir/bar.html')
      expect(rv).to have_key('spec/samples/dir/foo.rb')

      expect(rv['spec/samples/dir/bar.html']).to eq([7,18, 27, 7])
    end
  end
  describe 'nested dir' do
    before(:each) do
      @extractor = TimeSeriesExtractor.new
    end

    it 'can analyze a whole directory structure' do
      rv = @extractor.extract_time_series_from_dir('spec/samples/nested_dir')
      expect(rv.keys.size).to eq(4)
      expect(rv).to have_key('spec/samples/nested_dir/another.c')
      expect(rv).to have_key('spec/samples/nested_dir/bar.html')
      expect(rv).to have_key('spec/samples/nested_dir/foo.rb')
      expect(rv).to have_key('spec/samples/nested_dir/subdir/something.lisp')

    end
    it 'can analyze a whole directory structure, with exclusion patterns' do
      rv = @extractor.extract_time_series_from_dir('spec/samples/nested_dir', ['.*foo.*', '.*.html'])
      expect(rv.keys.size).to eq(2)
      expect(rv).to have_key('spec/samples/nested_dir/another.c')
      expect(rv).to have_key('spec/samples/nested_dir/subdir/something.lisp')

    end
  end

  describe 'printing for consumption' do

    before(:each) do
      @extractor = TimeSeriesExtractor.new
    end

    it 'can emit for consumption by another tool' do
      raw = { 'foo' => [11,22,33]}
      rv = @extractor.format_for_consumption(raw['foo'])
      expect(rv).to eq(['1, 11', '2, 22', '3, 33'])
    end


  end
  describe 'flattening' do

    before(:each) do
      @extractor = TimeSeriesExtractor.new
    end

    it 'can emit for consumption by another tool' do
      std = @extractor.extract_time_series_from_file('spec/samples/others/tabs.c')
      expect(std).to eq([12, 17, 23])

      trimmed = @extractor.extract_time_series_from_file('spec/samples/others/tabs.c', {:trim => true})
      expect(trimmed).to eq([7, 11, 22])

    end

    it 'can handle tabs' do
      expect(@extractor.extract(["\tA"], {:tabsize => 3})).to eq([4])
      expect(@extractor.extract(["\t\tBC"], {:tabsize => 4})).to eq([10])
    end


  end
end
