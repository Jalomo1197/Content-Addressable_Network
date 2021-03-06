# Content-Addressable Network
###### NOTE: still being developed
## Team Members

| Github | Website|
|------------- | -------------|
|  [Alex Jalomo](https://github.com/Jalomo1197)            |  [jalomo1197.github.io](https://jalomo1197.github.io/Portfolio/)            |
| [Deividas Mickevicius](https://github.com/davidmickev)            |  [davidmickev.github.io](https://davidmickev.github.io)            |
|   [Jacob Sanchez](https://github.com/jsanchez78)            |  [jacobsanchez.dev](https://www.jacobsanchez.dev)            |


## Prerequisites
Installed [sbt-1.4.1](https://www.scala-sbt.org/download.html) 

## Instructions
* Setup
    * Download the project [source](https://github.com/Jalomo1197/Content-Addressable_Network.git) 
    * or CL `git clone https://github.com/Jalomo1197/Content-Addressable_Network.git`
    * Open/import the project in your IDE:

* Options to run
    * `sbt clean compile run`
    
* Alternate method: Run the main Scala class `Driver.scala`

## Docker Instance (coming soon): 
    
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


* Current version of Akka

Documentations:

[CAN Algorithm](https://people.eecs.berkeley.edu/~sylvia/papers/cans.pdf)

[CAN Implementation](http://www.diva-portal.org/smash/get/diva2:836192/FULLTEXT01.pdf)

[Akka Actors](https://doc.akka.io/docs/akka/current/)


# Design Architecture and Implementations

We are using typed Akka Behaviors model system to interact and manipulate Nodes through actors.
The distributed data stored can be found in 'aplication.conf' which contains (key,value)->(Movie, "Year,Revenue") pairs.

The movie dataset that was used can be found on [kaggle](https://www.kaggle.com/rounakbanik/the-movies-dataset?select=movies_metadata.csv)
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

#### Implementations
 
To send immutable messages between typed actor models to construct and manipulate Nodes within CAN
Each can stores a chunk- called a zone of entire hash table containing multiple key value pairs
Each node holds information about a small number of adjacent zones in the table (neighbor table)
Can space is divided amongst nodes, it can grow incrementally by allocating its own portion of coordinate space to another node by splitting half of its allocated zone in half retaining half and giving other part to new node

[ 1 ] -> [ 1|2 ]  (Same Area)
1)	New node finds a node in CAN
2)	Find a node whose zone will be split
3)	Update neighbors on split so routing can include new node

Bootstrap – New CAN node can discover IP address of any node currently in system -> Use some bootstrap mechanism

Assume can has associated DNS and this resolves IP address of one or more CAN bootstrap nodes.

Bootstrap nodes maintain partial list of CAN nodes in the system

New node -> Looks up CAN domain name in DNS to retrieve boostrap IP address.

Bootsrap then supplies IP address of several random chosen nodes in system

Finding a zone – New node randomly chooses point P in space and sends a join request for destination P

Then node splits its zone in half and assigns one half to new node.

For 2-d space, zone is split in X then Y dimension. 

Then (key,value) pairs from the half zone are handed over to new node

Joining the routing (update) update neighbors to eliminate and update old / new neighbors

Node Leave - If no neighbors. It becomes empty space, otherwise if has 2 people in one node, its given to neighbor whose zone is smallest

When node fails/leaves/dies it initiates takeover mechanism and starts a takeover 
When timer expires a node sends TAKEOVER message conveying its own zone volume to all of failed node’s neighbors

On receipt of TAKEOVER msg, a node cancels its own timer if the zone volume in the message is smaller that its zone value or replies with its own takeover msg.

This just ensures neighbors are updates and neighboring node is chosen while still active/alive

Zone overloading - (repeated (key,value) pairs that are frequently accessed)

Advantages : reduced path length, number of hops, which is less latency, 
Improved fault tolerance because a zone is vacant only when all the nodes in a zone crash at same time
Negatives: overloading adds complexity because nodes must track set of peers

On join – send message to random point on space, the existing node in space knows its zone coordinates and those of its neighbors, and  instead of directly splitting zone, the first node compares the volume of its zone to its neighbors in the coordinate space to accommodate the split.

Total volume = V(t) and n is total number on nodes which will be assigned to a zone of voume V(t/n) to each node

Caching and replication techniques

Replication: A overloaded(work) node can replicate the key and data at each of neighboring nodes for load balancing

#### EVALUATION SYSTEMS
    *    Entry | Direction
    1.     0   | Left
    2.     1   | Up
    3.     2   | Right
    4.     3   | Down
    5.     4   | default (self)

#### RunTime 

For a d dimensional space partitioned into n equal zones, the average routing path length is (d/4)(^1/d) and individual nodes maintain 2d neighbors. And path length grows at O(n^1/d)

## Chord

Implementation details can be found in [previous project repository](https://bitbucket.org/jsanch75/group_13/src/master/).
The focus of this README.md discussed the implementation and design of [CAN](https://people.eecs.berkeley.edu/~sylvia/papers/cans.pdf).

