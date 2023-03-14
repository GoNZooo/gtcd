defmodule BencodingTest do
  use ExUnit.Case

  test "encodes strings correctly" do
    assert :bencoding.encode("spam") == {:ok, "4:spam"}
    assert :bencoding.encode("hello") == {:ok, "5:hello"}
  end

  test "encodes integers correctly" do
    assert :bencoding.encode(42) == {:ok, "i42e"}
    assert :bencoding.encode(0) == {:ok, "i0e"}
    assert :bencoding.encode(-42) == {:ok, "i-42e"}
  end

  test "encodes lists correctly" do
    assert :bencoding.encode(["spam", "eggs"]) == {:ok, "l4:spam4:eggse"}
    assert :bencoding.encode([1, 2, 3]) == {:ok, "li1ei2ei3ee"}
  end

  test "encodes maps correctly" do
    assert :bencoding.encode(%{"cow" => "moo", "spam" => "eggs"}) ==
             {:ok, "d3:cow3:moo4:spam4:eggse"}

    assert :bencoding.encode(%{"spam" => "eggs", "cow" => "moo"}) ==
             {:ok, "d3:cow3:moo4:spam4:eggse"}

    assert :bencoding.encode(%{"spam" => ["a", "b"]}) == {:ok, "d4:spaml1:a1:bee"}
    assert :bencoding.encode(%{"spam" => %{"a" => "b"}}) == {:ok, "d4:spamd1:a1:bee"}
  end

  test "decodes strings correctly" do
    assert :bencoding.decode("4:spam") == {:ok, %{value: "spam", rest: ""}}
    assert :bencoding.decode("5:hello") == {:ok, %{value: "hello", rest: ""}}
  end

  test "decodes integers correctly" do
    assert :bencoding.decode("i42e") == {:ok, %{value: 42, rest: ""}}
    assert :bencoding.decode("i0e") == {:ok, %{value: 0, rest: ""}}
    assert :bencoding.decode("i-42e") == {:ok, %{value: -42, rest: ""}}
  end

  test "decodes lists correctly" do
    assert :bencoding.decode("l4:spam4:eggse") == {:ok, %{value: ["spam", "eggs"], rest: ""}}
    assert :bencoding.decode("li1ei2ei3ee") == {:ok, %{value: [1, 2, 3], rest: ""}}
  end

  test "decodes maps correctly" do
    assert :bencoding.decode("d3:cow3:moo4:spam4:eggse") ==
             {:ok, %{value: %{"cow" => "moo", "spam" => "eggs"}, rest: ""}}

    assert :bencoding.decode("d3:cow3:moo4:spam4:eggse") ==
             {:ok, %{value: %{"spam" => "eggs", "cow" => "moo"}, rest: ""}}

    assert :bencoding.decode("d4:spaml1:a1:bee") ==
             {:ok, %{value: %{"spam" => ["a", "b"]}, rest: ""}}

    assert :bencoding.decode("d4:spamd1:a1:bee") ==
             {:ok, %{value: %{"spam" => %{"a" => "b"}}, rest: ""}}
  end
end
