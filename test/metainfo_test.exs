defmodule MetainfoTest do
  use ExUnit.Case

  require Logger

  def run_tests(module) do
    Logger.info("Running tests for #{inspect(module)}")
    tests = TestLib.find_tests(module)
    Logger.info("Found #{length(tests)} tests")
    Logger.info("Tests: #{inspect(tests)}")

    for name <- tests do
      Logger.info("Running test #{inspect(name)}")
      effect = apply(module, name, [])
      test_data = effect.()
      Logger.info("Test #{inspect(name)} result: #{inspect(test_data)}")

      case test_data do
        {:equalTest, %{left: left, right: right}} ->
          assert left == right

        {:lessThanTest, %{left: left, right: right}} ->
          assert left < right

        {:greaterThanTest, %{left: left, right: right}} ->
          assert left > right

        {:lessThanOrEqualTest, %{left: left, right: right}} ->
          assert left <= right

        {:greaterThanOrEqualTest, %{left: left, right: right}} ->
          assert left >= right

        {:notEqualTest, %{left: left, right: right}} ->
          assert left != right

        {:assertTest, %{value: value}} ->
          assert value

        {:refuteTest, %{value: value}} ->
          refute value
      end
    end
  end

  # run PS tests
  # test "main tests" do
  #   :test_main@ps.main().()
  # end

  test "metainfo" do
    run_tests(:test_gtcd_metaInfo@ps)
  end
end
