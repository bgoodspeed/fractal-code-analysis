require 'spec_helper'

require 'timeline_helper'
RSpec.describe TimelineHelper do
  describe 'graphics' do
    before(:each) do
      @helper = TimelineHelper.new
    end

    it 'can emit JS data structures' do
      rv = @helper.raw_data_for('spec/samples/timeline.txt')
      expect(rv.size).to eq(115)
      expect(rv.first.year).to eq(1949)
      expect(rv.last.year).to eq(2014)


    end
    it 'can emit JS data structures non raw' do
      tle = TimelineEntry.new
      tle.id = 123
      tle.year = 4444
      tle.author = 'foo bar'
      tle.title  = 'asdf wert'
      rv = @helper.emit_javascript_data_for('spec/samples/timeline.txt', [tle])
      expect(rv.size).to eq(1)
      expect(rv.first).to eq("{id: 123, content: 'foo bar, asdf wert', start: '4444-01-01'},")


    end
  end
end
