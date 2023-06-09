defmodule BencodingTest do
  use ExUnit.Case

  test "encodes strings correctly" do
    assert :gtcd_bencoding.encode("spam") == {:ok, "4:spam"}
    assert :gtcd_bencoding.encode("hello") == {:ok, "5:hello"}
  end

  test "encodes integers correctly" do
    assert :gtcd_bencoding.encode(42) == {:ok, "i42e"}
    assert :gtcd_bencoding.encode(0) == {:ok, "i0e"}
    assert :gtcd_bencoding.encode(-42) == {:ok, "i-42e"}
  end

  test "encodes lists correctly" do
    assert :gtcd_bencoding.encode(["spam", "eggs"]) == {:ok, "l4:spam4:eggse"}
    assert :gtcd_bencoding.encode([1, 2, 3]) == {:ok, "li1ei2ei3ee"}
  end

  test "encodes maps correctly" do
    assert :gtcd_bencoding.encode(%{"cow" => "moo", "spam" => "eggs"}) ==
             {:ok, "d3:cow3:moo4:spam4:eggse"}

    assert :gtcd_bencoding.encode(%{"spam" => "eggs", "cow" => "moo"}) ==
             {:ok, "d3:cow3:moo4:spam4:eggse"}

    assert :gtcd_bencoding.encode(%{"spam" => ["a", "b"]}) == {:ok, "d4:spaml1:a1:bee"}
    assert :gtcd_bencoding.encode(%{"spam" => %{"a" => "b"}}) == {:ok, "d4:spamd1:a1:bee"}
  end

  test "decodes strings correctly" do
    assert :gtcd_bencoding.decode("4:spam") == {:ok, %{value: "spam", rest: "", source: "4:spam"}}

    assert :gtcd_bencoding.decode("5:hello") ==
             {:ok, %{value: "hello", rest: "", source: "5:hello"}}
  end

  test "decodes integers correctly" do
    assert :gtcd_bencoding.decode("i42e") == {:ok, %{value: 42, rest: "", source: "i42e"}}
    assert :gtcd_bencoding.decode("i0e") == {:ok, %{value: 0, rest: "", source: "i0e"}}
    assert :gtcd_bencoding.decode("i-42e") == {:ok, %{value: -42, rest: "", source: "i-42e"}}
  end

  test "decodes lists correctly" do
    assert :gtcd_bencoding.decode("l4:spam4:eggse") ==
             {:ok, %{value: ["spam", "eggs"], rest: "", source: "l4:spam4:eggse"}}

    assert :gtcd_bencoding.decode("li1ei2ei3ee") ==
             {:ok, %{value: [1, 2, 3], rest: "", source: "li1ei2ei3ee"}}
  end

  test "decodes maps correctly" do
    assert :gtcd_bencoding.decode("d3:cow3:moo4:spam4:eggse") ==
             {:ok,
              %{
                value: %{"cow" => "moo", "spam" => "eggs"},
                rest: "",
                source: "d3:cow3:moo4:spam4:eggse"
              }}

    assert :gtcd_bencoding.decode("d3:cow3:moo4:spam4:eggse") ==
             {:ok,
              %{
                value: %{"spam" => "eggs", "cow" => "moo"},
                rest: "",
                source: "d3:cow3:moo4:spam4:eggse"
              }}

    assert :gtcd_bencoding.decode("d4:spaml1:a1:bee") ==
             {:ok, %{value: %{"spam" => ["a", "b"]}, rest: "", source: "d4:spaml1:a1:bee"}}

    assert :gtcd_bencoding.decode("d4:spamd1:a1:bee") ==
             {:ok, %{value: %{"spam" => %{"a" => "b"}}, rest: "", source: "d4:spamd1:a1:bee"}}
  end

  test "generates the correct metainfo hash for torrent files" do
    arch_torrent_file =
      :code.priv_dir(:gtcd) |> Path.join("torrents/archlinux-2023.03.01-x86_64.iso.torrent")

    {:ok, {:file_metainfo, %{info_hash: hash}}} =
      :gtcd_metainfo.parse_torrent_file(arch_torrent_file)

    assert hash == "%6E%BB%05%96%F1%6E%8D%CA%66%35%92%1C%5E%12%5F%29%3E%4C%EC%AD"
  end
end
