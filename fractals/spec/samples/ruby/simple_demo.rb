
# I am a comment after a blank line
class SimpleDemo
  def instance_method_1(arg1, arg2)
    "#{arg1},#{arg2}"
  end

  def self.class_method_1(another_optional_argument = nil)
    10.times do |i|
      long_var_name = i + 3 # this is a wasted computation
      block_assigned = long_var_name.to_a.collect do |element|
        element.to_s
      end

      block_assigned.length
    end

  end

end