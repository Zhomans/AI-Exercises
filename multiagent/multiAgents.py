# multiAgents.py
# Editted by Zach Homans, Submitted 5/3/13
# --------------
# Licensing Information: Please do not distribute or publish solutions to this
# project. You are free to use and extend these projects for educational
# purposes. The Pacman AI projects were developed at UC Berkeley, primarily by
# John DeNero (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# For more info, see http://inst.eecs.berkeley.edu/~cs188/sp09/pacman.html

from util import manhattanDistance
from game import Directions
import random, util
import math
import itertools

from game import Agent

class ReflexAgent(Agent):
  """
    A reflex agent chooses an action at each choice point by examining
    its alternatives via a state evaluation function.

    The code below is provided as a guide.  You are welcome to change
    it in any way you see fit, so long as you don't touch our method
    headers.
  """


  def getAction(self, gameState):
    """
    You do not need to change this method, but you're welcome to.

    getAction chooses among the best options according to the evaluation function.

    Just like in the previous project, getAction takes a GameState and returns
    some Directions.X for some X in the set {North, South, West, East, Stop}
    """
    # Collect legal moves and successor states
    legalMoves = gameState.getLegalActions()

    # Choose one of the best actions
    scores = [self.evaluationFunction(gameState, action) for action in legalMoves]
    bestScore = max(scores)
    bestIndices = [index for index in range(len(scores)) if scores[index] == bestScore]
    chosenIndex = random.choice(bestIndices) # Pick randomly among the best

    "Add more of your code here if you want to"

    return legalMoves[chosenIndex]

  def evaluationFunction(self, currentGameState, action):
    """
    Design a better evaluation function here.

    The evaluation function takes in the current and proposed successor
    GameStates (pacman.py) and returns a number, where higher numbers are better.

    The code below extracts some useful information from the state, like the
    remaining food (oldFood) and Pacman position after moving (newPos).
    newScaredTimes holds the number of moves that each ghost will remain
    scared because of Pacman having eaten a power pellet.

    Print out these variables to see what you're getting, then combine them
    to create a masterful evaluation function.
    """
    # Useful information you can extract from a GameState (pacman.py)
    successorGameState = currentGameState.generatePacmanSuccessor(action)
    newPos = successorGameState.getPacmanPosition()
    oldFood = currentGameState.getFood().asList()
    newGhostStates = successorGameState.getGhostStates()
    newScaredTimes = [ghostState.scaredTimer for ghostState in newGhostStates]

    "*** YOUR CODE HERE ***"
    #Simply takes Manhattan Distances of Food and Ghosts into account and subtracts them.

    foodTotal = 0
    for food in oldFood:
      dist = manhattanDistance(newPos,food)
      foodTotal += 1.0/(dist+1)

    ghostTotal = 0
    for ghost in newGhostStates:
      dist = manhattanDistance(ghost.getPosition(), newPos)
      if dist == 0:
        ghostTotal = 1000
        break
      else:
        ghostTotal += (1.0/(dist+1))

    return (foodTotal)-(ghostTotal)


def scoreEvaluationFunction(currentGameState):
  """
    This default evaluation function just returns the score of the state.
    The score is the same one displayed in the Pacman GUI.

    This evaluation function is meant for use with adversarial search agents
    (not reflex agents).
  """
  return currentGameState.getScore()

class MultiAgentSearchAgent(Agent):
  """
    This class provides some common elements to all of your
    multi-agent searchers.  Any methods defined here will be available
    to the MinimaxPacmanAgent, AlphaBetaPacmanAgent & ExpectimaxPacmanAgent.

    You *do not* need to make any changes here, but you can if you want to
    add functionality to all your adversarial search agents.  Please do not
    remove anything, however.

    Note: this is an abstract class: one that should not be instantiated.  It's
    only partially specified, and designed to be extended.  Agent (game.py)
    is another abstract class.
  """

  def __init__(self, evalFn = 'scoreEvaluationFunction', depth = '2'):
    self.index = 0 # Pacman is always agent index 0
    self.evaluationFunction = util.lookup(evalFn, globals())
    self.depth = int(depth)

class MinimaxAgent(MultiAgentSearchAgent):
  """
    Your minimax agent (question 2)
  """

  def getAction(self, gameState):
    """
      Returns the minimax action from the current gameState using self.depth
      and self.evaluationFunction.

      Here are some method calls that might be useful when implementing minimax.

      gameState.getLegalActions(agentIndex):
        Returns a list of legal actions for an agent
        agentIndex=0 means Pacman, ghosts are >= 1

      Directions.STOP:
        The stop direction, which is always legal

      gameState.generateSuccessor(agentIndex, action):
        Returns the successor game state after an agent takes an action

      gameState.getNumAgents():
        Returns the total number of agents in the game
    """
    "*** YOUR CODE HERE ***"
  
    def minimax(node,depth,currentAgent):

      #Return the heuristic if depth or win is reached.
      if depth <= 0 or node[1].isWin():
        return (node[0], self.evaluationFunction(node[1]))

      #Alpha is positive or negative based on the currentAgent.
      alpha = (Directions.STOP, (999999*(1-2*(currentAgent==0))))
      
      #Find Successor States.
      children = []
      for action in node[1].getLegalActions(currentAgent):
        children.append((action,node[1].generateSuccessor(currentAgent,action)))
      
      #If there's no successors, just return the heuristic.
      if children == []:
        return (node[0],self.evaluationFunction(node[1]))

      #Find the child score of each child and then find the max or min of these scores accordingly.
      while children != []:
        child = children.pop(0)

        score = (child[0], minimax(child,depth-1*int(currentAgent==(node[1].getNumAgents()-1)),(currentAgent+1)%node[1].getNumAgents())[1])
        
        if currentAgent == 0:
          if alpha[1] == None or score[1] > alpha[1]:
            alpha = score
        else:
          if alpha[1] == None or score[1] < alpha[1]:
            alpha = score
      return alpha


    mm = minimax((None,gameState),self.depth,0)

    #A problem this program has is that it counts the moves such as (North, Stop) and (Stop, North) as the same score-wise, but chooses to do (Stop,North) since it get calculated first.
    #This means that it picks to move "Stop" this turn. It does the same calculation next frame, so it "Stop"s again. It does this until disturbed by a ghost.
    #I've determined that this is just a result of the heuristic presented, and not a flaw in my code.
    return mm[0]



class AlphaBetaAgent(MultiAgentSearchAgent):
  """
    Your minimax agent with alpha-beta pruning (question 3)
  """

  def getAction(self, gameState):
    """
      Returns the minimax action using self.depth and self.evaluationFunction
    """
    "*** YOUR CODE HERE ***"
    #Basically works exactly the same as MinimaxAgent except that it tracks both an alpha and beta. It has the same issues with not moving.
    def alphabeta(node, depth, alpha, beta, currentAgent):
      if currentAgent>(node[1].getNumAgents()-1):
        currentAgent = 0
      if depth == 0 or node[1].isWin():
          return (node[0],self.evaluationFunction(node[1]))

      children = []
      for action in node[1].getLegalActions(currentAgent):
        children.append((action,node[1].generateSuccessor(currentAgent,action)))

      if children == []:
        return (node[0],self.evaluationFunction(node[1]))

      if currentAgent == 0:
        for child in children:
          newAlpha = (child[0],alphabeta(child, depth-1*int(currentAgent==(node[1].getNumAgents()-1)), alpha, beta, currentAgent+1)[1])
          if alpha[1]==None or newAlpha[1] > alpha[1]:
            alpha = newAlpha
          if beta[1]!= None and beta[1] <= alpha[1]:
            break      
        return alpha
      else:
        for child in children:
          newBeta = (child[0],alphabeta(child, depth-1*int(currentAgent==(node[1].getNumAgents()-1)), alpha, beta, currentAgent+1)[1])
          if beta[1]==None or newBeta[1] < beta[1]:
            beta = newBeta
          if alpha[1]==None and beta[1] <= alpha[1]:
            break
        return beta

    ab = alphabeta((None, gameState), self.depth, (Directions.STOP,None), (Directions.STOP,None), 0)
    return ab[0]


class ExpectimaxAgent(MultiAgentSearchAgent):
  """
    Your expectimax agent (question 4)
  """

  def getAction(self, gameState):
    """
      Returns the expectimax action using self.depth and self.evaluationFunction

      All ghosts should be modeled as choosing uniformly at random from their
      legal moves.
    """
    "*** YOUR CODE HERE ***"
    #This is also very similar to Minimax. 
    def expectimax(node, depth, currentAgent):
      if currentAgent>(node[1].getNumAgents()-1):
        currentAgent = 0
      if depth == 0 or node[1].isWin():
          return (node[0],self.evaluationFunction(node[1]))

      children = []
      for action in node[1].getLegalActions(currentAgent):
        children.append((action,node[1].generateSuccessor(currentAgent,action)))

      if children == []:
        return (node[0],self.evaluationFunction(node[1]))

      if currentAgent == 0:
        alpha = (Directions.STOP,-99999999)
        for child in children:
          score = (child[0],expectimax(child, depth-1*int(currentAgent==(node[1].getNumAgents()-1)), currentAgent+1)[1])
          if score[1] > alpha[1]:
            alpha = score
      #The only difference is here, where alpha is just the sum of the children Expectimaxes.
      else:
        alpha = (node[0],0)
        for child in children:
            alpha = (alpha[0], alpha[1] + ((1.0/len(children)) * expectimax(child, depth-1*int(currentAgent==(node[1].getNumAgents()-1)), currentAgent+1)[1]))
      return alpha

    em = expectimax((None,gameState),self.depth,0)
    return em[0]

def betterEvaluationFunction(currentGameState):
  """
    Your extreme ghost-hunting, pellet-nabbing, food-gobbling, unstoppable
    evaluation function (question 5).

    DESCRIPTION: <write something here so we know what you did>
  """
  "*** YOUR CODE HERE ***"
  #Very simple heuristic, but it seems to work well. If I were to spend more time, I'd use a breath-first search to determine the nearest pellet and tend in that direction.
  oldFood = currentGameState.getFood().asList()
  pos = currentGameState.getPacmanPosition()
  foodTotal = 0
  for food in oldFood:
    dist = manhattanDistance(pos,food)
    foodTotal += 1.0/(dist+1)

  return currentGameState.getScore()+foodTotal


# Abbreviation
better = betterEvaluationFunction

class ContestAgent(MultiAgentSearchAgent):
  """
    Your agent for the mini-contest
  """

  def getAction(self, gameState):
    """
      Returns an action.  You can use any method you want and search to any depth you want.
      Just remember that the mini-contest is timed, so you have to trade off speed and computation.

      Ghosts don't behave randomly anymore, but they aren't perfect either -- they'll usually
      just make a beeline straight towards Pacman (or away from him if they're scared!)
    """
    "*** YOUR CODE HERE ***"
    util.raiseNotDefined()

