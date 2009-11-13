# vim: ft=ruby

### Enable syntax highlighting and wirble magic
require 'rubygems'
require 'wirble'

Wirble.init
Wirble.colorize

### Object.subclasses

class Object
  def self.subclasses(direct = false)
    classes = []
    if direct
      ObjectSpace.each_object(Class) do |c|
        next unless c.superclass == self
        classes << c
      end
    else
      ObjectSpace.each_object(Class) do |c|
        next unless c.ancestors.include?(self) and (c != self)
        classes << c
      end
    end
    classes
  end
end

### Easily print methods local to an object's class
class Object
  def local_methods
    (methods - Object.instance_methods).sort
  end
end
