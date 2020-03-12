# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here

  # your enhancements here

 # class array holding all the pieces and their rotations
  def self.next_piece (board)
    if board.is_cheating 
      board.turnoff_cheat
      # puts (board.is_cheating)
      board.set_score(board.get_score - 100)
      MyPiece.new([[[0, 0]]], board) 
    else 
      MyPiece.new(All_My_Pieces.sample, board)
    end 
    # puts "hello, world"c
  end

  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # z
               rotations([[0, 0], [0, 1], [1, 0], [-1, 0], [-1, 1]]), # new piece 1
               [[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]],
               [[0, 2], [0, 1], [0, 0], [0, -1], [0, -2]]], # new piece 2
               rotations([[0, 0], [1, 0], [0, 1]])] # new piece 3
end

class MyBoard < Board
  # your enhancements here

  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
    @cheating = false 
  end

  def get_score 
    @score 
  end 

  def set_score(s)
    @score = s
  end 

  def next_piece
    @current_block = MyPiece.next_piece(self)
    @current_pos = nil    
  end

  def cheat
    # if !game_over? and @game.is_running?
    # if !game_over?
    if @score >= 100
      @cheating = true 
    end 
    # end
  end

  def is_cheating
    @cheating 
  end 

  def turnoff_cheat 
    # if !game_over? 
    @cheating = false 
  end 

#  gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size-1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
  # your enhancements here

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings 
    super 
    @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
    @root.bind('c', proc {@board.cheat})
  end

end


