class GameBoard
    attr_reader :max_row, :max_column, :arr, :num_sc
    $num_sc = 0;
    def initialize(max_row, max_column)
      @max_row = max_row
      $num_sc = 0
        @max_column = max_column
        @arr = Array.new(max_row){Array.new(max_column)}
        @arr.each_index do |row|
          @arr[row].each_index do |col|
            @arr[row][col] = String.new
          end
        end
    end

    # adds a Ship object to the GameBoard
    # returns Boolean
    # Returns true on successfully added the ship, false otherwise
    # Note that Position pair starts from 1 to max_row/max_column
    def add_ship(ship)
      len = ship.size
      if (ship.start_position.row > max_row || ship.start_position.column > max_column || ship.start_position.row < 1 || ship.start_position.column < 1) then
        return false
      end
      
      if (arr[ship.start_position.row-1][ship.start_position.column-1].include?('s')) then
        return false
      end


      if ship.orientation == "Right" then
        ship.size.times do |x|
          if (ship.start_position.column + x > max_column || ship.start_position.column + x < 1) then
            return false
          end
          if arr[ship.start_position.row-1][ship.start_position.column-1 + x].include?('s') then
            return false
          end
        end
      end
      if ship.orientation == "Left" then
        ship.size.times do |x|
          if (ship.start_position.column - x > max_column || ship.start_position.column - x < 1) then
            return false
          end
          if arr[ship.start_position.row-1][ship.start_position.column-1 - x].include?('s') then
            return false
          end
        end
      end

      if ship.orientation == "Up" then
        ship.size.times do |x|
          if (ship.start_position.row - x > max_column || ship.start_position.row - x < 1) then
            return false
          end
          if arr[ship.start_position.row-1-x][ship.start_position.column-1].include?('s') then
            return false
          end
        end
      end
      if ship.orientation == "Down" then
        ship.size.times do |x|
          if (ship.start_position.row + x > max_column || ship.start_position.row + x < 1) then
            return false
          end
          if arr[ship.start_position.row-1+x][ship.start_position.column-1].include?('s') then
            return false
          end
        end
      end
            

      if ship.orientation == "Right" then
        ship.size.times do |x|
          arr[ship.start_position.row-1][ship.start_position.column-1 + x].prepend("s")
        end
      end
      if ship.orientation == "Left" then
        ship.size.times do |x|
          arr[ship.start_position.row-1][ship.start_position.column-1 - x].prepend("s")
        end
      end
      if ship.orientation == "Up" then
        ship.size.times do |x|
          arr[ship.start_position.row-1-x][ship.start_position.column-1].prepend("s")
        end
      end
      if ship.orientation == "Down" then
        ship.size.times do |x|
          arr[ship.start_position.row-1+x][ship.start_position.column-1].prepend("s")
        end
      end

      return true
    end
    # return Boolean on whether attack was successful or not (hit a ship?)
    # return nil if Position is invalid (out of the boundary defin
    def attack_pos(position)
      # check position
      if (position.row < 1 || position.column < 1 || position.row > max_row || position.column > max_column) then
        return nil
      end
      if (arr[position.row-1][position.column-1].include?("s") == false) then
        arr[position.row-1][position.column-1].prepend("b");
        return false
      end
      if (arr[position.row-1][position.column-1].include?("b") == false) then
        $num_sc = $num_sc + 1
      end
      
      arr[position.row-1][position.column-1].prepend("b");
      return true
    end
    

    # Number of successful attacks made by the "opponent" on this player GameBoard
    def num_successful_attacks
        return $num_sc
    end

    # returns Boolean
    # returns True if all the ships are sunk.
    # Return false if at least one ship hasn't sunk.
    def all_sunk?
      @arr.each do |rows|
        rows.each do |cols|
          if cols.include?("s") == true && cols.include?("b") == false then
            return false
          end
        end
      end
      return true
    end


    # String representation of GameBoard (optional but recommended)
    def to_s

      str = String.new
      @arr.each do |r|
        r.each do |c|
          str += "["
          str += c
          str += "]"
        end
        str+= "\n"
      end
        return str
    end
end
