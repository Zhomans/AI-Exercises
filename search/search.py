# search.py
# Edited by Zach Homans. Submitted 4/26/13.
# ---------
# Licensing Information: Please do not distribute or publish solutions to this
# project. You are free to use and extend these projects for educational
# purposes. The Pacman AI projects were developed at UC Berkeley, primarily by
# John DeNero (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# For more info, see http://inst.eecs.berkeley.edu/~cs188/sp09/pacman.html

"""
In search.py, you will implement generic search algorithms which are called 
by Pacman agents (in searchAgents.py).
"""

import util

class SearchProblem:
  """
  This class outlines the structure of a search problem, but doesn't implement
  any of the methods (in object-oriented terminology: an abstract class).
  
  You do not need to change anything in this class, ever.
  """
  
  def getStartState(self):
     """
     Returns the start state for the search problem 
     """
     util.raiseNotDefined()
    
  def isGoalState(self, state):
     """
       state: Search state
    
     Returns True if and only if the state is a valid goal state
     """
     util.raiseNotDefined()

  def getSuccessors(self, state):
     """
       state: Search state
     
     For a given state, this should return a list of triples, 
     (successor, action, stepCost), where 'successor' is a 
     successor to the current state, 'action' is the action
     required to get there, and 'stepCost' is the incremental 
     cost of expanding to that successor
     """
     util.raiseNotDefined()

  def getCostOfActions(self, actions):
     """
      actions: A list of actions to take
 
     This method returns the total cost of a particular sequence of actions.  The sequence must
     be composed of legal moves
     """
     util.raiseNotDefined()
           

def tinyMazeSearch(problem):
  """
  Returns a sequence of moves that solves tinyMaze.  For any other
  maze, the sequence of moves will be incorrect, so only use this for tinyMaze
  """
  from game import Directions
  s = Directions.SOUTH
  w = Directions.WEST
  return  [s,s,w,s,w,w,s,w]

def depthFirstSearch(problem):
  """
  Search the deepest nodes in the search tree first [p 85].
  
  Your search algorithm needs to return a list of actions that reaches
  the goal.  Make sure to implement a graph search algorithm [Fig. 3.7].
  
  To get started, you might want to try some of these simple commands to
  understand the search problem that is being passed in:
  
  print "Start:", problem.getStartState()
  print "Is the start a goal?", problem.isGoalState(problem.getStartState())
  print "Start's successors:", problem.getSuccessors(problem.getStartState())
  """
  "*** YOUR CODE HERE ***"
  solutionStack = util.Stack() #Tracks solution in a stack. Could have used a list as well.

  myStack = util.Stack()
  myStack.push([problem.getStartState(), [],[]])

  #Cycles through search tree.
  while not myStack.isEmpty():
    currentStateInfo = myStack.pop()
    currentState = currentStateInfo[0]
    previousStates = currentStateInfo[1]
    directionList = currentStateInfo[2]

    if problem.isGoalState(currentState):
      solutionStack.push(directionList) #If a solution works, put in the solutionStack.
    else:
      for successor in problem.getSuccessors(currentState):
        if successor[0] not in previousStates:
          newPreviousStates = list(previousStates) #list() is used to copy and not overwrite the previous previousStates.
          newPreviousStates.append(currentState)
          newDirectionList = list(directionList) #Same as above.
          newDirectionList.append(successor[1])
          myStack.push([successor[0],newPreviousStates,newDirectionList])

  #Find the optimal solution in the solutionStack.
  optimalSolution = solutionStack.pop()
  while not solutionStack.isEmpty():
    nextSolution = solutionStack.pop()
    if len(nextSolution) < len(optimalSolution):
      optimalSolution = nextSolution

  return optimalSolution

def breadthFirstSearch(problem):
  "Search the shallowest nodes in the search tree first. [p 81]"
  "*** YOUR CODE HERE ***"
  myQueue = util.Queue()
  myQueue.push([problem.getStartState(), []])
  previousStates = dict() #Dictionaries are faster when using the in function.

  while not myQueue.isEmpty():
    currentStateInfo = myQueue.pop()
    currentState = currentStateInfo[0]
    directionList = currentStateInfo[1]

    if problem.isGoalState(currentState):
      return directionList #As so as a solution is found, it must be the best.
    else:
      for successor in problem.getSuccessors(currentState):
        if successor[0] not in previousStates:
          previousStates[successor[0]] = True #Arbitrary value.
          newDirectionList = list(directionList)
          newDirectionList.append(successor[1])
          myQueue.push([successor[0],newDirectionList])

      
def uniformCostSearch(problem):
  "Search the node of least total cost first. "
  "*** YOUR CODE HERE ***"
  #Same comments as bfs.
  myPQueue = util.PriorityQueue()
  myPQueue.push([problem.getStartState(), [], 0],0)
  previousStates = dict()

  while not myPQueue.isEmpty():
    currentStateInfo = myPQueue.pop()
    currentState = currentStateInfo[0]
    directionList = currentStateInfo[1]
    totalCost = currentStateInfo[2]

    if problem.isGoalState(currentState):
      return directionList
    else:
      for successor in problem.getSuccessors(currentState):
        if successor[0] not in previousStates:
          previousStates[successor[0]] = True
          newDirectionList = list(directionList)
          newDirectionList.append(successor[1])
          newTotalCost = totalCost
          newTotalCost += successor[2]
          myPQueue.push([successor[0],newDirectionList,newTotalCost],newTotalCost)

def nullHeuristic(state, problem=None):
  """
  A heuristic function estimates the cost from the current state to the nearest
  goal in the provided SearchProblem.  This heuristic is trivial.
  """
  return 0

def aStarSearch(problem, heuristic=nullHeuristic):
  "Search the node that has the lowest combined cost and heuristic first."
  "*** YOUR CODE HERE ***"
  #Same comments as bfs.
  myPQueue = util.PriorityQueue()
  myPQueue.push([problem.getStartState(), [], 0],0)
  previousStates = dict()

  while not myPQueue.isEmpty():
    currentStateInfo = myPQueue.pop()
    currentState = currentStateInfo[0]
    directionList = currentStateInfo[1]
    totalCost = currentStateInfo[2]

    if problem.isGoalState(currentState):
      return directionList
    else:
      for successor in problem.getSuccessors(currentState):
        if successor[0] not in previousStates:
          previousStates[successor[0]] = True
          newDirectionList = list(directionList)
          newDirectionList.append(successor[1])
          newTotalCost = totalCost
          newTotalCost += successor[2]
          myPQueue.push([successor[0],newDirectionList,newTotalCost],newTotalCost+heuristic(successor[0],problem))
   
  
# Abbreviations
bfs = breadthFirstSearch
dfs = depthFirstSearch
astar = aStarSearch
ucs = uniformCostSearch