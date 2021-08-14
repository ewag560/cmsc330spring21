require_relative '../models/game_board'
require_relative '../models/ship'
require_relative '../models/position'

# return a populated GameBoard or nil
# Return nil on any error (validation error or file opening error)
# If 5 valid ships added, return GameBoard; return nil otherwise
def read_ships_file(path)
  g = GameBoard.new 10, 10
  $shipsAdded = 0
  read_file_lines(path) { |line|
    if line =~ /^[(]\d+,\d+[)], (Right|Left|Up|Down), [1-5]$/ && $shipsAdded < 5 then
      pos = Position.new(line.scan(/\d+/).first.to_i, line.scan(/\d+/)[1].to_i) 
      ship = Ship.new(pos, line.scan(/\w+/)[2], line.scan(/\d+/)[2].to_i)
      g.add_ship(ship)
      $shipsAdded = $shipsAdded+1
    end
  }
  if $shipsAdded == 5 then
    return g
  end
  return nil
end


# return Array of Position or nil
# Returns nil on file open error
def read_attacks_file(path)
  arr = Array.new
  if read_file_lines(path) == false then
    return nil
  end
  read_file_lines(path) { |line|
    if line =~ /^[(]\d+,\d+[)]$/ then
      pos = Position.new(line.scan(/\d+/).first.to_i, line.scan(/\d+/)[1].to_i)
      arr = arr<<pos
    end
  }
  return arr
end


# ===========================================
# =====DON'T modify the following code=======
# ===========================================
# Use this code for reading files
# Pass a code block that would accept a file line
# and does something with it
# Returns True on successfully opening the file
# Returns False if file doesn't exist
def read_file_lines(path)
    return false unless File.exist? path
    if block_given?
        File.open(path).each do |line|
            yield line
        end
    end

    true
end
