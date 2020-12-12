# CS441_Fall2020_Final_Project
[Repository Link](https://bitbucket.org/jsanch75/group_13/src/master/)

# Group Members
Deividas Mickevicius : dmicke5

Jacob Sanchez : jsanch75

Alex Jalomo : ajalom2

# Prerequisites
Installed [sbt-1.4.1](https://www.scala-sbt.org/download.html) 

# Instructions
* Setup
    * Download the project [source](https://bitbucket.org/jsanch75/group_13/src/master/) 
    * or CL `git clone https://mttdavid@bitbucket.org/jsanch75/group_13_courseproject.git`
    * Open/import the project in your IDE:

* Options to run
    * `sbt clean compile run`
    
* Alternate method: Run the main Scala class `Driver.Simulation`

# Docker Instance: 

### Prerequisites
    
Install [Docker](https://www.docker.com/get-started)

1. Open docker CLI
2. Download project from [source]() 
3. docker run -it (Image-Name)
4. `sbt clean compile run` 

# Citation
Software and design is based on: 

* Content-Addressable Network (CAN) as a distributed infrastructure that provides hash table-like functionality on Internet-like
scales. The CAN is scalable, fault-tolerant and completely self-organizing,
and we demonstrate its scalability, robustness and low-latency properties
through simulation.

* Chord: A Scalable Peer-to-peer Lookup Service for Internet Applications

* Current version of Akka

The source to the documentation can be found at:

https://people.eecs.berkeley.edu/~sylvia/papers/cans.pdf

http://www.diva-portal.org/smash/get/diva2:836192/FULLTEXT01.pdf

https://pdos.csail.mit.edu/papers/chord:sigcomm01/chord_sigcomm.pdf

https://doc.akka.io/docs/akka/current/

# Visualization with R for Chord

Utilizing the Circilize methodology we map the values from the configuration file into R representing which location each Movie gets assigned to
the chord. There were 5 different algorithm procedures in R all utilizing the SHA-1 methodology to encrypt the name.
The results on the image become very pixelated as the size of entries get larger.

![Alt text](Rplot.png?raw=true "Title")


# Design Architecture and Implementations

We are using typed Akka Behaviors model system to interact and manipulate Nodes through actors.
The distributed data stored can be found in 'aplication.conf' which contains (key,value)->(Movie, "Year,Revenue") pairs.

The movies dataset that was used can be found on [kaggle](https://www.kaggle.com/rounakbanik/the-movies-dataset?select=movies_metadata.csv)
and was parsed with the parser.py script located under PythonParser that takes the first valid 250+ movies that have a title and budget/year.

Actors that simulate nodes in the simulated cloud have corresponding hash values are generated using unique names that will be assigned to these nodes and they will be inserted based on those hashes




## Content-Addressable Network (CAN)

Our implementation creates a 2-dimensional [0, 16.00]x[0, 16.00] coordinate space for the CAN. 
The first node to join the CAN becomes the owner of the entire CAN space. For every new node that enters, if a zone is split, that node becomes the owner of that space.
                
#### Abstractions: Procedures
Our implementation includes via Akka Actor messages/commands to accomplish functionalities 
such as:
 
 • The insertion of movie data
 
 • The retrieval of movie data
 
 • The insertion of a new node into the CAN
 
Such functionalities require much distinct and indistinct information. 
The trivial way to ensure critical information is present, is to add numerous 
parameters to distinct messages/commands. 
In order to reduce commands having numerous parameters, 
the case class Procedure[T] is introduced in our implementation. 

A Procedure instance essentially encapsulates all required information for 
a given procedures. A procedure can be the rout or actions taken to produce
the mentioned functionalities. Information can be extracted form the Procedure 
instances once the destination has been reached. The extraction of information 
can be unsafe if the Procedure instance does not encapsulate required information.
The future version of Procedure will try to enforce required data at compile time via
Scala phantom types. CHORD in the future will be reimplemented to take advantage 
of the generality and clearness that Procedure gives.                         

#### Architecture And Construction Of A CAN
Our architecture and construction of a CAN overlay consists of three steps:
##### Entities And Responsibilities:
###### DNS Actor
   • Singleton Actor that simulations the entry point for a given User Actor.
   
   • Communication point for user to insert and retrieve data.
   
   • DNS forward data insertion and data retrievals procedures to a bootstrap node.

###### Bootstrap Actor
   • First bootstrap node is responsible for the first four zones/nodes initializations.
   
   • A bootstrap node forwards data insertion and data retrievals procedures 
    to an arbitrary active node in the network.
   
   • A bootstrap node reboots failed CAN nodes for which it is responsible for.

###### CAN Node Actor 
   • Determines optimal next neighbor to forward procedure if its zone does not contain the desired destination.

   • Updates/Initializes its neighbors_table appropriately on command.

   • Send a split procedure to itself when congested, meaning the node is responsible for to much data.

    
##### Overlay routing.
   • The nodes are able to route messages in the CAN overlay utilizing only information about neighbouring nodes
and their zones. 
   
   • Since the CAN space is a 2-dimensional coordinate grid, this becomes a matter of routing along a straight
line. (Vector (x,x),(y,y) to P)


### Further Overview CAN Driver

We use typed Akka model system to interact and simulate Content Addressable Network.
Our (CAN) is a robust, scalable, distributed systems designed for efficient search of data
stored in a DHT. 

IDEA : Sending immutable messages between typed actor models to construct and manipulate Nodes within CAN

 1. Akka/HTTP abstraction created through DNS. 
 2. User simulates Insertions and Queries of data to DNS by requesting actions
 3. FILL IN REST
 4.
 5.
 6.
  

## Chord

Implementation details can be found in [previous project repository](https://bitbucket.org/jsanch75/group_13/src/master/).
The focus of this README.md discussed the implementation and design of [CAN](https://people.eecs.berkeley.edu/~sylvia/papers/cans.pdf).

