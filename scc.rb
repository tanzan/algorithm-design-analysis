rawg = [[1,7], [4,1], [7,4], [7,9], [9,6],
  [3,9], [6,3], [6,8], [8,2], [2,5],
  [5,8]]

def graph(rawg)
  g = {}
  rawg.each { |e|
    g[e[0]]=[] unless g[e[0]]
    g[e[0]].push e[1]
  }
  return g
end

def raw_inverse(rawg)
  invg = []
  rawg.each { |e| invg.push [e[1],e[0]] }
  return invg
end

def vertices(rawg)
  vs = {}
  rawg.flatten.each { |v|
    vs[v] = false  
  }
  return vs.to_a
end

def dfs(g, v, fts, ls, t, s, hop, m)
  m[v[0]] = true
  hop.push v[0]
  ls[v[0]] = s[0]
  lt = t
  if g[v[0]]
    g[v[0]].each { |j|
      lt = dfs(g, [j,false], fts, ls, lt, s, hop, m) unless m[j]        
    }
  end
  lt+=1
  fts[v[0]] = lt
  return lt 
end

def dfs_loop(g,vs)
  t = 0
  s = 0
  ls = {}
  fts = {}
  hops = []
  m = []
  vs.reverse.each { |v|    
    s = v
    hop = []
    t = dfs(g, v, fts, ls, t, s, hop, m) unless m[v[0]]
    hops.push hop unless hop.empty?      
  }
  return hops, fts, ls
end

def scc(rawg)
  
  invg = raw_inverse rawg
  
  hops, fts, ls = dfs_loop graph(invg), vertices(invg)
 
  invfts = []
  
  fts.keys.each { |v|
    invfts.push [fts[v], v]    
  }
  
  vs = []
  
  invfts.sort.each { |p|
    vs.push [p[1],false]    
  } 
    
  hops, tls = dfs_loop graph(rawg), vs
  
  return hops
end


puts scc(rawg).inspect

File.open('SCC.txt') { |f|
  g = []
  f.each { |l|
    e = l.split(' ')
    g.push [e[0].to_i, e[1].to_i]   
  }
 sz = []
 scc(g).each {|sc| sz.push sc.size if sc.size > 1}
 puts sz.sort[-10..-1].reverse.inspect
}
