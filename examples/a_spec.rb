require "failure"

describe "A failing spec" do

  it "recurses 5 times and then fails" do
    lambda { recurse(5) }.should_not raise_error
  end

end
