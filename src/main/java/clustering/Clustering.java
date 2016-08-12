package clustering;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 *
 * @author serg
 */
public class Clustering {
    
    public static class Vertex {
        
        private int id;
        private Cluster cluster;

        public Vertex(int id) {
            this.id = id;
            this.moveTo(new Cluster());
        }

        public int getId() {
            return id;
        }
        
        public final void moveTo(Cluster cluster){
            if (this.cluster != null){
                this.cluster.removeVertex(this);
            }
            this.cluster = cluster;
            if (this.cluster != null){
                this.cluster.addVertex(this);
            }
        }

        @Override
        public int hashCode() {
            int hash = 3;
            hash = 37 * hash + this.id;
            return hash;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final Vertex other = (Vertex) obj;
            if (this.id != other.id) {
                return false;
            }
            return true;
        }

        @Override
        public String toString() {
            return "Vertex{" + "id=" + id + '}';
        }
        
        

        public Cluster getCluster() {
            return this.cluster;
        }
    }
    
    public static class HVertex extends Vertex implements Comparable<HVertex> {

        public HVertex(int id) {
            super(id);
        }
        
        public static int countBits(int id){
            int n = 0;
            while(id > 0){
                ++n;
                id &= id - 1;
            }
            return n;
        }

        @Override
        public int compareTo(HVertex o) {
            return countBits(this.getId()) - countBits(o.getId());
        }
        
    }
    
    public static class Cluster {
        
        private List<Vertex> members = new ArrayList<Vertex>();
        
        public final void addVertex(Vertex vertex){
            members.add(vertex);
        }
        
        public final void removeVertex(Vertex vertex){
            members.remove(vertex);
        }
        
        public List<Vertex> getMembers(){
            return Collections.unmodifiableList(members);
        }
        
        public void merge(Cluster other){
            List<Vertex> membersCopy = new ArrayList<Vertex>();
            membersCopy.addAll(other.getMembers());
            for(Vertex v : membersCopy){
                v.moveTo(this);
            }
        }

        @Override
        public String toString() {
            return "Cluster{" + "members=" + members + '}';
        }
        
        
        
    }
    
    public static class Edge implements Comparable<Edge> {
 
        private Vertex from;
        private Vertex to;
        private int length;

        public Edge(Vertex from, Vertex to, int length) {
            this.from = from;
            this.to = to;
            this.length = length;
        }

        public Vertex getFrom() {
            return from;
        }

        public Vertex getTo() {
            return to;
        }

        public int getLength() {
            return length;
        }

        @Override
        public int compareTo(Edge other) {
            return length - other.length;
        }

        @Override
        public int hashCode() {
            int hash = 5;
            hash = 97 * hash + (this.from != null ? this.from.hashCode() : 0);
            hash = 97 * hash + (this.to != null ? this.to.hashCode() : 0);
            hash = 97 * hash + this.length;
            return hash;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) {
                return false;
            }
            if (getClass() != obj.getClass()) {
                return false;
            }
            final Edge other = (Edge) obj;
            if (this.from != other.from && (this.from == null || !this.from.equals(other.from))) {
                return false;
            }
            if (this.to != other.to && (this.to == null || !this.to.equals(other.to))) {
                return false;
            }
            if (this.length != other.length) {
                return false;
            }
            return true;
        }

        @Override
        public String toString() {
            return "Edge{" + "from=" + from + ", to=" + to + ", length=" + length + '}';
        }
        
        
        
    }
    
    
    public static class Graph {
        
        private List<Vertex> vertexes;
        private List<Edge> edges;
        private LinkedList<Edge> minHeap = new LinkedList<Edge>();
        
        public Graph(List<Vertex> vertexes, List<Edge> edges){
            this.edges = edges;
            this.vertexes = vertexes;
            minHeap.addAll(edges);
            Collections.sort(minHeap);
        }
        
        public List<Vertex> getVertexes(){
            return Collections.unmodifiableList(vertexes);            
        }
        
        public List<Edge> getEdges(){
            return Collections.unmodifiableList(edges);
        }
        
        public Edge getMinEdge(){
            return minHeap.poll();
        }
        
        private static String[] readLines(String fileName) throws IOException {
            FileInputStream is = new  FileInputStream(fileName);
            try{
                byte[] buf = new byte[1024];
                StringBuilder sb = new StringBuilder();
                for( ; ; ){
                    int n = is.read(buf);
                    if (n == -1){
                        break;
                    }
                    sb.append(new String(buf,0,n)); 
                }
                return sb.toString().split("\\n");
     
            } finally {
                is.close();
            }
        }
        
        public static Graph read(String fileName) throws IOException {
            String[] lines = readLines(fileName);
            List<Edge> edges = new ArrayList<Edge>();
            Map<Integer,Vertex> vertexMap = new HashMap<Integer,Vertex>();
            for(int i=1; i < lines.length; ++i){
                String[] edge = lines[i].split("\\s+");
                Vertex from = vertexMap.get(Integer.parseInt(edge[0])); 
                if (from == null){
                    from = new Vertex(Integer.parseInt(edge[0]));
                    vertexMap.put(from.id,from);
                }
                Vertex to = vertexMap.get(Integer.parseInt(edge[1]));
                if (to == null){
                    to = new Vertex(Integer.parseInt(edge[1]));
                    vertexMap.put(to.id, to);
                }
                edges.add(new Edge(from, to, Integer.parseInt(edge[2])));
            }
            return new Graph(new ArrayList<Vertex>(vertexMap.values()), edges);
        }
        
        
        public static Graph readHypercube(String fileName, int maxS) throws IOException{
            String[] lines = readLines(fileName);
            Map<Integer,HVertex> vertexMap = new HashMap<Integer,HVertex>();
            for(int i=1; i < lines.length ; ++i){
                int id = Integer.parseInt(lines[i].replace(" ", ""),2);
                if (vertexMap.get(id) == null){
                    vertexMap.put(id,new HVertex(id));
                }
            }
            List<HVertex> vertexList = new ArrayList<HVertex>(vertexMap.values());
            Collections.sort(vertexList);
            List<Edge> edges = new ArrayList<Edge>();
            Map<Integer,Vertex> vertexFilter = new HashMap<Integer,Vertex>();
            for(HVertex v1 : vertexList){
                for(HVertex v2 : vertexList){
                    int len = HVertex.countBits(v1.getId() ^ v2.getId());
                    if (len > maxS){
                        continue;
                    }
                    if (vertexFilter.get(v1.getId()) == null){
                        vertexFilter.put(v1.getId(), v1);
                    }
                     if (vertexFilter.get(v2.getId()) == null){
                        vertexFilter.put(v2.getId(), v2);
                    }
                    edges.add(new Edge(v1, v2, len));
                }
            }            
            return new Graph(new ArrayList<Vertex>(vertexFilter.values()),edges);
        }
        
    }
    
    
    public static int kClustering(Graph graph, int k, List<Cluster> clusters, int maxS){
        for(Vertex v : graph.vertexes){
            clusters.add(v.cluster);
        }
        int s = 0;
        while(clusters.size() > k){
            Edge edge = graph.getMinEdge();
            if (edge == null){
                return s;
            }
            if (edge.from.getCluster() == edge.to.getCluster()){
                continue;
            }
            s = edge.getLength();
            if (s >= maxS){
                return s;
            }
            System.out.println(s);
            clusters.remove(edge.to.getCluster());
            edge.from.getCluster().merge(edge.to.getCluster());
            
        }
        return s;
    }
    
    
    public static void main(String[] args) throws IOException {
        List<Cluster> clusters = new ArrayList<Clustering.Cluster>();
        //System.out.println("S=" + kClustering(Graph.readFromFile("../test.txt"), 4, clusters));
        //System.out.println("S=" + kClustering(Graph.read("../clustering1.txt"), 3, clusters, Integer.MAX_VALUE));
        System.out.println("S=" + kClustering(Graph.readHypercube("clustering_big.txt", 3), 1 , clusters, 3));
        for(Cluster c : clusters){
            System.out.println(c.toString());
            System.out.println(c.members.size());
        }
        System.out.println("K=" + clusters.size());
        //System.out.println(HVertex.countBits(3));
    }
}
