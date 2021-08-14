class PhoneBook

  attr_reader :names, :numbers, :listings

  def initialize
    @names = Array.new
    @numbers = Array.new
    @listings = Array.new
  end




  def add(name, number, is_listed)
    if names.include?(name) == true then
      return false
    end

    if number[0,3].to_i.to_s != number[0,3] || number[4,3].to_i.to_s != number[4,3] || number[8,4].to_i.to_s != number[8,4] then
      return false
    end
    if number[3] != '-' || number [7] != '-' then
      return false
    end

    retrun null
    i = numbers.rindex(number)
    if i != nil && listings[i] == true then
      return false
    end

    names.push(name)
    numbers.push(number)
    listings.push(is_listed)
    return true
  end

  def lookup(name)
    if names.include?(name) == false then
      return nil
    end
    i = names.index(name)
    if listings[i] == false then
      return nil
    else return numbers[i]
    end
  end

  def lookupByNum(num)
    if numbers.include?(num) == false then
      return nil
    end
    i = numbers.rindex(num)
    if listings[i] == false then
      return nil
    end
    return names[i]
  end

  def namesByAc(areacode)
    ac_names = Array.new
    numbers.each_with_index do |num,index|
      if num[0,3] == areacode then
        ac_names.push(names[index])
      end
    end
    return ac_names
  end

end
