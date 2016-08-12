

class Vertex

  attr_reader :id
  attr_reader :cluster

  def initialize(id)
    @id = id
    @cluster = Cluster.new(self)
  end

  def move_to(cluster)
    @cluster = cluster
  end

  def inspect
    "Vertex(#{@id})"
  end

  def ==(other)
    @id == other.id
  end

end

class Edge

  include Comparable

  attr_reader :from
  attr_reader :to
  attr_reader :length

  def initialize(from,to, length)
    @from = from
    @to = to
    @length = length
  end

  def inspect
    "Edge(#{@from.inspect},#{@to.inspect},#{@length})"
  end

  def <=>(other)
    @length <=> other.length
  end

end


class Graph

  attr_reader :vertexes
  attr_reader :edges

  def initialize(vertexes, edges)
    @vertexes = vertexes
    @edges = edges
    @edge_heap = edges.sort.reverse
  end

  def get_min_edge
    @edge_heap.pop
  end

end

class Cluster

  attr_reader :members

  def initialize(vertex)
    @members = []
    @members.push vertex
    @id = vertex.id
  end

  def merge(cluster)
    cluster.members.each {|v|
      v.move_to self
      @members.push v
    }
  end

  def inspect
    "Cluster(#{@members.inspect})"
  end

end

def k_clustering(graph, k)

  clusters = []

  graph.vertexes.each { |v|
    clusters.push v.cluster
  }

  s = 0

  while clusters.size > k
    e = graph.get_min_edge
    s = e.length
    clusters.delete e.to.cluster
    e.from.cluster.merge e.to.cluster
    puts e.from.cluster.inspect  if e.from.cluster.members.size > 2
  end

  return s, clusters

end

vertexes = [Vertex.new(1), Vertex.new(2), Vertex.new(3), Vertex.new(4)]
edges = [Edge.new(vertexes[0],vertexes[1], 1), Edge.new(vertexes[2], vertexes[3], 2),
         Edge.new(vertexes[0],vertexes[2], 3), Edge.new(vertexes[0], vertexes[3], 4),
         Edge.new(vertexes[1],vertexes[2], 5), Edge.new(vertexes[1], vertexes[3], 6)]

puts k_clustering(Graph.new(vertexes, edges), 3).inspect

def add_vertex(vertexes, vertex)
  vertexes.push vertex unless vertexes.include? vertex
end

def read_graph(filename)
  edges = []
  vertexes = []
  File.open(filename){ |file|
    file.readlines.each { |line|
      edge = line.split(/\s+/)
      edge.delete("")
      if edge.size == 3
            from = Vertex.new(edge[0].to_i)
            to = Vertex.new(edge[1].to_i)
            edges.push Edge.new(from, to, edge[2].to_i)
            add_vertex(vertexes, from)
            add_vertex(vertexes, to)
      end
    }
  }
  puts vertexes.size
  return Graph.new(vertexes,edges)
end

puts k_clustering(read_graph("clustering1.txt"),4).inspect






