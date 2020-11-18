# CS441_Fall2020_HW3
[Repository Link](https://bitbucket.org/jsanch75/group_13/src/master/)

#Group Members
Deividas Mickevicius : dmicke5 <br>
Jacob Sanchez : jsanch75 <br>
Alex Jalomo : ajalom2 <br>

#Prerequisites
Installed [sbt-1.3.13](https://www.scala-sbt.org/download.html) 

#Instructions
* Download the project [source](https://bitbucket.org/jsanch75/group_13/src/master/) or use

`git clone https://mttdavid@bitbucket.org/jsanch75/group_13.git`

* Open/import the project in your IDE:

* Open Terminal and build the project using SBT

`sbt clean compile run`

* Alternate method: Run the main Scala class 
``scala
Driver
``

#Citation
Software and design was based on: Chord: A Scalable Peer-to-peer Lookup Service for Internet Applications and current version of akka.
The source to the documentation can be found at:  <br>
https://pdos.csail.mit.edu/papers/chord:sigcomm01/chord_sigcomm.pdf <br>
https://doc.akka.io/docs/akka/current/


#Design Architecture and Implementations
The data that is stored in the Chord will be a list of movie data.
The key(movieName) node will store the values(year,$revenue) of that movie. <br>
The movies dataset that was used can be found on [kaggle](https://www.kaggle.com/rounakbanik/the-movies-dataset?select=movies_metadata.csv)
and was parsed with the parser.py script located under /group_13/ that takes the first valid 250+ movies that have a title and budget/year.

####Each bulletin will describe the implementation of the System Model
Load balance: Chord acts as a distributed hash function,
spreading keys evenly over the nodes; this provides a degree
of natural load balance.

* The application.conf contains N entries for the chord.
* For each item or for a subset of items we `join` the nodes

