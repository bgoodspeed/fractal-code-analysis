require 'spec_helper'

require 'mccabe_helper'
RSpec.describe MccabeHelper do
  describe 'generates average mccabe complexity' do
    before(:each) do
      @helper = MccabeHelper.new
    end

    it 'can extract all measurements for all functions with the pmccabe tool' do
      rv = @helper.raw_data_for('spec/samples/c/main.c')
      expect(rv.size).to eq(1)
      expect(rv.first.first).to eq('1')
      expect(rv.first[1]).to eq('1')

    end

    it 'can extract larger files' do
      rv = @helper.raw_data_for('spec/samples/c/audio.c')
      expect(rv.size).to eq(73)
      expect(rv[1][1]).to eq('24')
    end

    it 'can compute sum' do
      rv = @helper.mccabe_sum_for('spec/samples/c/audio.c')

      expect(rv).to eq(604)
    end

  end
end
