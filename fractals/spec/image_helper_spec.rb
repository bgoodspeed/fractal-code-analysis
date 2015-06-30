require 'spec_helper'

require 'image_helper'
RSpec.describe ImageHelper do
  describe 'graphics' do
    before(:each) do
      @helper = ImageHelper.new
    end

    it 'can extract all files in a directory' do
      rv = @helper.line_segment_points_for('spec/samples/images/demo.bmp')
      expect(rv.size).to eq(774)
      expect(rv.first).to eq(:asdf)
      expect(rv.first.first).to eq(123)
      expect(rv.first.last).to eq(1332)

    end
  end
end
