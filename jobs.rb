
class Job
  include Comparable

  attr :weight
  attr :len
  attr_reader :ctime
  attr_writer :ctime

  def initialize(weight, len)
    @weight = weight
    @len = len
    @ctime = len
  end

  def priority
    @weight - @len
  end

  def <=> (other)
    s = -(priority <=> other.priority)
    if s == 0
      return -(weight <=> other.weight)
    end
    return s
  end

  def inspect
    "Job(#{@weight},#{@len},#{priority})"
  end

end

class Job2 < Job
  def priority
    @weight.to_f / @len.to_f
  end

  def <=> (other)
    -(priority <=> other.priority)
  end
end

jobs = [Job.new(8,50),Job.new(74 ,59),Job.new(31 ,73)]


puts jobs.sort.inspect

def run_jobs(jobs)
  t = 0
  jobs.sort!.each { |job|
    job.ctime = job.ctime + t
    t += job.len
  }
  t = 0
  jobs.each { |job|
    t += job.ctime * job.weight
  }
  puts jobs[0..10].inspect
  puts t
end

File.open("jobs.txt") { |file|
  jobs = []
  jobs2 = []
  file.readlines.each { |line|
    j = line.split(/\s+/)
    j.delete("")
    if j.size == 2
      jobs.push Job.new(j[0].to_i,j[1].to_i)
      jobs2.push Job2.new(j[0].to_i,j[1].to_i)
    end

  }
  run_jobs jobs
  run_jobs jobs2
}


