class MyRange
  include Enumerable
  def initialize(low,high)
    @low = low
    @high = high
  end
  def each
    i=@low
    while i <= @high
      yield i
      i=i+1
    end
  end
end


# print(MyRange.new(4,2).any?{|i| i<=4})

class A
  def initialize a
    @arr = a
  end
  def get i
    @arr[i]
  end
  def sum
    @arr.inject(0) {|acc,x| acc + x}
    # i = 0
    # s = 0
    # while i <= (@arr.length() -1)
    #   s = s + @arr[i]
    #   i = i + 1
    # end 
    # s 
  end
end

class B < A
  def initialize a
    super
    @ans = false
  end
  def sum
    if !@ans
      @ans = @arr.inject(0) {|acc,x| acc + x}
    end
    @ans
  end
  # def sum
  #   # @arr.inject(0) {|acc,x| acc + x}
  #   if !@ans 
  #     i = 0
  #     @ans = 0
  #     while i <= (@arr.length() -1)
  #       @ans = @ans + @arr[i]
  #       i = i + 1
  #     end 
  #   end 
  #   @ans 
  # end

end


v = [4,19,74]
a = A.new v
b = B.new v
s1 = a.sum
s2 = b.sum

s3 = a.sum
s4 = b.sum
print(s3)
print(s4)