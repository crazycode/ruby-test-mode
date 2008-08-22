$:.unshift(File.dirname(__FILE__))
require "failure"

describe "A failing spec" do

  it "recurses 5 times and then fails" do
    recurse(5)
  end
  
  it "this is a simple failure" do
    "this".should == "that"
  end
end
