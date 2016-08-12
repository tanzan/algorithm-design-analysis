require 'algorithms'
include Containers

class Edge

  attr :vertex
  attr :weight

  def initialize(vertex,weight)
    @vertex = vertex
    @weight = weight
  end

end

class Vertex

  include Comparable

  attr_reader :id
  attr_reader :adjacent
  attr_accessor :key
  attr_accessor :parent
  attr_accessor :weight

  def initialize(id)
    @id = id
    @adjacent = []
    @parent = nil
    @key = 999999999999999999999999999999999999999999999999999999
    @weight = 0
  end

  def link_to(vertex,weight)
    @adjacent.push(Edge.new(vertex,weight))
  end

  def <=>(other)

    if @key.nil? && other.key.nil?
      return 0
    end

    if @key.nil?
      return 1
    elsif other.nil?
      return -1
    end

    return @key <=> other.key

  end

  def inspect
    "Vertex(#{@id},#{@key},#{@parent.inspect})"
  end

end


def mst_prim(graph)

  graph[0].key = 0;

  m_heap = MinHeap.new(graph)
  mst = []
  r = []
  vl = 0
  graph.each { |v| r.push v.id}

  while !m_heap.empty?
    v = m_heap.pop
    mst.push(v)
    vl += v.key
    r.delete(v.id)
    v.adjacent.each { |e|
      if r.include?(e.vertex.id) && (e.weight < e.vertex.key)
        e.vertex.key = e.weight
        e.vertex.parent = v
        m_heap.delete(e.vertex)
        m_heap.push(e.vertex)
      end
    }
  end

  puts vl
  mst


end


def link(v1, v2, w)
  v1.link_to v2, w
  v2.link_to v1, w
end

v1 = Vertex.new(1)
v2 = Vertex.new(2)
v3 = Vertex.new(3)
v4 = Vertex.new(4)

link v1, v2, 1
link v2, v4, 1
link v2, v3, 2
link v3, v4, 3
link v4, v1, 4

g = [v1,v2,v3,v4]

mst_prim(g).each { |v|
  puts v.inspect
}

File.open("edges.txt") { |file|
  g = []
  v_map = {}
  file.readlines.each { |line|
    e = line.split(/\s+/)
    e.delete("")
    if e.size == 3

      v1 = v_map[e[0].to_i]

      if v1.nil?
        v1 = Vertex.new(e[0].to_i)
        g.push v1
        v_map[e[0].to_i] = v1
      end

      v2 = v_map[e[1].to_i]
      if v2.nil?
        v2 = Vertex.new(e[1].to_i)
        g.push v2
        v_map[e[1].to_i] = v2
      end

      link v1, v2, e[2].to_i

    end

  }

  mst_prim(g).each { |v|
    #puts v.inspect
  }
}


