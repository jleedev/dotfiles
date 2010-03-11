# vim: ft=ruby

### Enable syntax highlighting and wirble magic
require 'rubygems'
require 'wirble'

Wirble.init
Wirble.colorize

### Object.subclasses

class Object
  def self.subclasses(direct = false)
    if direct
      ObjectSpace.each_object(Class).select do |c|
        c.superclass == self
      end
    else
      ObjectSpace.each_object(Class).select do |c|
        c.ancestors.include?(self) and (c != self)
      end
    end
  end
end

### Easily print methods local to an object's class
class Object
  def local_methods
    (methods - Object.instance_methods).sort
  end
end
