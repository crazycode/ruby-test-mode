require "test/unit"
require "failure"

class FailingTest < Test::Unit::TestCase

  def test_recurse_and_then_fails
    assert_nothing_raised recurse(5)
  end

end
