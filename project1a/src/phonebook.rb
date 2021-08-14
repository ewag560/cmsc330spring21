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

    if number[0,3].to_i.to_s != number[0,3] || (number[4] =~ /[0-9]/) == false || (number[5] =~ /[0-9]/) == false || (number[6] =~ /[0-9]/) == false ||
       (number[8] =~ /[0-9]/) == false || (number[9] =~ /[0-9]/) == false || (number[10] =~ /[0-9]/) == false || (number[11] =~ /[0-9]/) == false then
      return false
    end


    if number[3] != '-' || number [7] != '-' then
      return false
    end

    numbers.each_with_index{|val,id|
      if val == number && listings[id] == true && is_listied == true then
        return false
      end
    }

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
      return nil else
      return numbers[i]
    end
  end

  def lookupByNum(number)
    if numbers.include?(number) == false then
      return nil
    end
    i = numbers.rindex(number)
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
