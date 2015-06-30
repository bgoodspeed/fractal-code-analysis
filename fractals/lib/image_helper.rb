require 'RMagick'

class ImageHelper

  include Magick
  def line_segment_points_for(filename)
    img = ImageList.new(filename)
    rv = []
    dataset = {}

    img.each_pixel do |pixel, c,r|


      if pixel.red < 30000 and pixel.green < 30000 and pixel.blue < 30000
        dataset[c] = [] unless dataset.has_key? c

        dataset[c] << r
      end


    end

    dataset.keys.each {|col|  rv << [dataset[col].min, col] }

    rv
  end

  def draw_overlay_on(filename, prefix='udpated-')
    img = ImageList.new(filename)
    segments = line_segment_points_for(filename)
    canvas = Magick::ImageList.new
    canvas.new_image(img[0].rows, img[0].columns) { self.background_color = 'white' }
    timeseries = Magick::Draw.new

    segments.each_with_index do |lineseg, idx|
      next if (idx + 1) >= segments.length

      timeseries.line( lineseg[0], lineseg[1], segments[idx + 1][0], segments[idx + 1][1])
    end

    timeseries.draw(canvas)
    canvas.write("#{prefix}#{filename}")

  end
end

if $0 == __FILE__
  raise "usage: #{$0} image.png" unless ARGV.length > 0
  ImageHelper.new.draw_overlay_on(ARGV.first)
end