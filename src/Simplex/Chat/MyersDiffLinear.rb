# https://blog.jcoglan.com/2017/04/25/myers-diff-in-linear-space-implementation/ß

class MyersLinear
    def self.diff(a, b)
      new(a, b).diff
    end
  
    def initialize(a, b)
      @a, @b = a, b
    end
  
    def diff
        diff = []
    
        walk_snakes do |x1, y1, x2, y2|
          if x1 == x2
            diff << Diff::Edit.new(:ins, nil, @b[y1])
          elsif y1 == y2
            diff << Diff::Edit.new(:del, @a[x1], nil)
          else
            diff << Diff::Edit.new(:eql, @a[x1], @b[y1])
          end
        end
    
        diff
      end

    def find_path(left, top, right, bottom)
        box   = Box.new(left, top, right, bottom)
        snake = midpoint(box)
    
        return nil unless snake
    
        start, finish = snake
    
        head = find_path(box.left, box.top, start[0], start[1])
        tail = find_path(finish[0], finish[1], box.right, box.bottom)
    
        (head || [start]) + (tail || [finish])
      end    

      def walk_snakes(&block)
        path = find_path(0, 0, @a.size, @b.size)
        return unless path
    
        path.each_cons(2) do |(x1, y1), (x2, y2)|
          x1, y1 = walk_diagonal(x1, y1, x2, y2, &block)
    
          case x2 - x1 <=> y2 - y1
          when -1
            yield x1, y1, x1, y1 + 1
            y1 += 1
          when 1
            yield x1, y1, x1 + 1, y1
            x1 += 1
          end
    
          walk_diagonal(x1, y1, x2, y2, &block)
        end
      end
    
      def walk_diagonal(x1, y1, x2, y2, &block)
        while x1 < x2 and y1 < y2 and @a[x1].text == @b[y1].text
          yield x1, y1, x1 + 1, y1 + 1
          x1, y1 = x1 + 1, y1 + 1
        end
        [x1, y1]
      end

      def midpoint(box)
        return nil if box.size == 0
    
        max = (box.size / 2.0).ceil
    
        vf    = Array.new(2 * max + 1)
        vf[1] = box.left
        vb    = Array.new(2 * max + 1)
        vb[1] = box.bottom
    
        (0 .. max).step do |d|
          forwards(box, vf, vb, d) { |snake| return snake }
          backward(box, vf, vb, d) { |snake| return snake }
        end
      end
    
      def forwards(box, vf, vb, d)
        (-d .. d).step(2).reverse_each do |k|
          c = k - box.delta
    
          if k == -d or (k != d and vf[k - 1] < vf[k + 1])
            px = x = vf[k + 1]
          else
            px = vf[k - 1]
            x  = px + 1
          end
    
          y  = box.top + (x - box.left) - k
          py = (d == 0 || x != px) ? y : y - 1
    
          while x < box.right and y < box.bottom and @a[x].text == @b[y].text
            x, y = x + 1, y + 1
          end
    
          vf[k] = x
    
          if box.delta.odd? and c.between?(-(d - 1), d - 1) and y >= vb[c]
            yield [[px, py], [x, y]]
          end
        end
      end

    # The backward method is much the same, except that it tries to minimise y rather than maximise x, and values in the vb array are indexed by c rather than k.
      def backward(box, vf, vb, d)
        (-d .. d).step(2).reverse_each do |c|
          k = c + box.delta
    
          if c == -d or (c != d and vb[c - 1] > vb[c + 1])
            py = y = vb[c + 1]
          else
            py = vb[c - 1]
            y  = py - 1
          end
    
          x  = box.left + (y - box.top) + k
          px = (d == 0 || y != py) ? x : x + 1
    
          while x > box.left and y > box.top and @a[x - 1].text == @b[y - 1].text
            x, y = x - 1, y - 1
          end
    
          vb[c] = y
    
          if box.delta.even? and k.between?(-d, d) and x <= vf[k]
            yield [[x, y], [px, py]]
          end
        end
      end

  end

  Box = Struct.new(:left, :top, :right, :bottom) do
    def width
      right - left
    end

    def height
      bottom - top
    end

    def size
      width + height
    end

    def delta
      width - height
    end
  end  