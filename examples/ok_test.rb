require "test/unit"

class OkTest < Test::Unit::TestCase
  def test_succeeds
    assert_equal "ok", :ok.to_s
  end
end
