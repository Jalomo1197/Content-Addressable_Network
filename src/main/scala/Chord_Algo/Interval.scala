package Chord_Algo

object Interval{

  def apply(start: Int, end: Int): Interval = new Interval(start, end)

  /*  Note: Range(s,e) => [s,e) */
  /*  Note: Largest n_id is  =>  2^(m - bit value) - 1 */

  /*  [s, e] circle interval of the network */
  def contains_FullInclusive(start: Int, end: Int, n_id: Int, m: Int): Boolean ={
    if (start < end){
      val interval = Range(start, end + 1)                  // [start, end]
      interval.contains(n_id)
    }
    else{
      val interval1 = Range(start, math.pow(2,m).toInt)     // [start, Largest n_id]
      val interval2 = Range(0, end + 1)                     // [0, end]
      interval1.contains(n_id) || interval2.contains(n_id)
    }
  }

  /* (s, e) circle interval of the network */
  def contains_FullExclusive(start: Int, end: Int, n_id: Int, m: Int): Boolean ={
    if (start < end){
      val interval = Range(start + 1, end)                   // (start, end)
      interval.contains(n_id)
    }
    else{
      val interval1 = Range(start + 1, math.pow(2,m).toInt)  // (start, Largest n_id]
      val interval2 = Range(0, end)                          // [0, end)
      interval1.contains(n_id) || interval2.contains(n_id)
    }
  }

  /* [s, e) circle interval of the network */
  def contains_InclusiveLeft(start: Int, end: Int, n_id: Int, m: Int): Boolean ={
    if (start < end){
      val interval = Range(start, end)                   // [start, end)
      interval.contains(n_id)
    }
    else{
      val interval1 = Range(start, math.pow(2,m).toInt)  // [start, Largest n_id]
      val interval2 = Range(0, end)                      // [0, end)
      interval1.contains(n_id) || interval2.contains(n_id)
    }
  }

  /* (s, e] circle interval of the network */
  def contains_InclusiveRight(start: Int, end: Int, n_id: Int, m: Int): Boolean ={
    if (start < end){
      val interval = Range(start + 1, end + 1)               // (start, end]
      interval.contains(n_id)
    }
    else{
      val interval1 = Range(start + 1, math.pow(2,m).toInt)  // (start, Largest n_id]
      val interval2 = Range(0, end + 1)                      // [0, end]
      interval1.contains(n_id) || interval2.contains(n_id)
    }
  }
}

// [a,b)
// Left side is inclusive && Right is exclusive
class Interval(start: Int, var end: Int) {
  def contains(nodeID: Int): Boolean = {
    val interval = Range(start, end)
    interval.contains(nodeID)
  }
  def get_end: Int =
    end
  def set_end(i: Int): Unit = {
    this.end = i
  }
}
