package Chord

// [a,b)
// Left side is inclusive && Right is exclusive
object Interval{
  def apply(start: BigInt, end: BigInt): Interval = new Interval(start, end)
}
class Interval(start: BigInt, end: BigInt) {
  def valid(nodeID: BigInt): Boolean = {
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
  def get_end: BigInt =
    end
}
