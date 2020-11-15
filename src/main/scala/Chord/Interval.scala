package Chord

// [a,b)
// Left side is inclusive && Right is exclusive
object Interval{
  def apply(start: Int, end: Int): Interval = new Interval(start, end)
}
class Interval(start: Int, end: Int) {
  def valid(nodeID: Int): Boolean = {
    if(start == end && nodeID == start){     // Single Node
      true
    }
    else if(start < end){
      if(nodeID >= start && nodeID < end)
        true
      else
        false
    }
    else
      false
  }
  def get_end: Int =
    end
}
