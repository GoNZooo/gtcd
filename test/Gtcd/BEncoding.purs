module Test.Gtcd.BEncoding
  ( testSuite
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Erl.Data.List (nil, (:))
import Erl.Data.List as List
import Erl.Data.Map as Map
import Gtcd.BEncoding as BEncoding
import Gtcd.MetaInfo (AnnounceData(..))
import Gtcd.MetaInfo as MetaInfo
import Partial.Unsafe as UnsafePartial
import PurerlTest (assertEqual, suite, test)
import PurerlTest.Types (Suites)

testSuite :: Suites
testSuite = do
  suite "BEncoding encoding and decoding" do
    test "Encodes `Int`s properly" do
      assertEqual (BEncoding.encode 42) "i42e"
      assertEqual (BEncoding.encode (-42)) "i-42e"

    test "Encodes `String`s properly" do
      assertEqual (BEncoding.encode "foo") "3:foo"
      assertEqual (BEncoding.encode "bar") "3:bar"
      assertEqual (BEncoding.encode "baz") "3:baz"
      assertEqual (BEncoding.encode "spam") "4:spam"

    test "Encodes `Array`s properly" do
      assertEqual (BEncoding.encode [ 1, 2, 3 ]) "li1ei2ei3ee"
      assertEqual (BEncoding.encode [ "foo", "bar", "baz" ]) "l3:foo3:bar3:baze"

    test "Encodes `List`s properly" do
      assertEqual (BEncoding.encode (1 : 2 : 3 : nil)) "li1ei2ei3ee"
      assertEqual
        (BEncoding.encode ("foo" : "bar" : "baz" : nil))
        "l3:foo3:bar3:baze"

    test "Encodes `Map`s properly" do
      assertEqual
        (BEncoding.encode (Map.fromFoldable [ "foo" /\ 1, "bar" /\ 2, "baz" /\ 3 ]))
        "d3:bari2e3:bazi3e3:fooi1ee"

      assertEqual
        (BEncoding.encode (Map.fromFoldable [ "foo" /\ "bar", "baz" /\ "spam" ]))
        "d3:baz4:spam3:foo3:bare"

      assertEqual
        (BEncoding.encode (Map.fromFoldable [ "cow" /\ "moo", "spam" /\ "eggs" ]))
        "d3:cow3:moo4:spam4:eggse"

      assertEqual
        (BEncoding.encode (Map.fromFoldable [ "spam" /\ "eggs", "cow" /\ "moo" ]))
        "d3:cow3:moo4:spam4:eggse"

    test "Parses `Int`s properly" do
      result <- liftEffect $ BEncoding.runParserM BEncoding.parser "i42e"
      assertEqual result (Right { result: 42, source: "i42e" })

      result2 <- liftEffect $ BEncoding.runParserM BEncoding.parser "i-42e"
      assertEqual result2 (Right { result: -42, source: "i-42e" })

    test "Parses `String`s properly" do
      result <- liftEffect $ BEncoding.runParserM BEncoding.parser "3:foo"
      assertEqual result (Right { result: "foo", source: "3:foo" })

      result2 <- liftEffect $ BEncoding.runParserM BEncoding.parser "3:bar"
      assertEqual result2 (Right { result: "bar", source: "3:bar" })

      result3 <- liftEffect $ BEncoding.runParserM BEncoding.parser "3:baz"
      assertEqual result3 (Right { result: "baz", source: "3:baz" })

      result4 <- liftEffect $ BEncoding.runParserM BEncoding.parser "4:spam"
      assertEqual result4 (Right { result: "spam", source: "4:spam" })

    test "Parses `Array`s properly" do
      result <- liftEffect $ BEncoding.runParserM BEncoding.parser "li1ei2ei3ee"
      assertEqual result (Right { result: [ 1, 2, 3 ], source: "li1ei2ei3ee" })

      result2 <- liftEffect $ BEncoding.runParserM BEncoding.parser "l3:foo3:bar3:baze"
      assertEqual
        result2
        (Right { result: [ "foo", "bar", "baz" ], source: "l3:foo3:bar3:baze" })

    test "Parses `List`s properly" do
      result <- liftEffect $ BEncoding.runParserM BEncoding.parser "li1ei2ei3ee"
      assertEqual
        result
        (Right { result: 1 : 2 : 3 : nil, source: "li1ei2ei3ee" })

      result2 <- liftEffect $ BEncoding.runParserM BEncoding.parser "l3:foo3:bar3:baze"
      assertEqual
        result2
        (Right { result: "foo" : "bar" : "baz" : nil, source: "l3:foo3:bar3:baze" })

    test "Parses `Map`s properly" do
      result <- liftEffect $ BEncoding.runParserM BEncoding.parser "d3:bari2e3:bazi3e3:fooi1ee"
      assertEqual
        result
        ( Right
            { result: Map.fromFoldable [ "foo" /\ 1, "bar" /\ 2, "baz" /\ 3 ]
            , source: "d3:bari2e3:bazi3e3:fooi1ee"
            }
        )

      result2 <- liftEffect $ BEncoding.runParserM BEncoding.parser "d3:baz4:spam3:foo3:bare"
      assertEqual
        result2
        ( Right
            { result: Map.fromFoldable [ "foo" /\ "bar", "baz" /\ "spam" ]
            , source: "d3:baz4:spam3:foo3:bare"
            }
        )

      result3 <- liftEffect $ BEncoding.runParserM BEncoding.parser "d3:cow3:moo4:spam4:eggse"
      assertEqual
        result3
        ( Right
            { result: Map.fromFoldable [ "cow" /\ "moo", "spam" /\ "eggs" ]
            , source: "d3:cow3:moo4:spam4:eggse"
            }
        )

      result4 <- liftEffect $ BEncoding.runParserM BEncoding.parser "d4:spam4:eggs3:cow3:mooe"
      assertEqual
        result4
        ( Right
            { result: Map.fromFoldable [ "cow" /\ "moo", "spam" /\ "eggs" ]
            , source: "d4:spam4:eggs3:cow3:mooe"
            }
        )

  suite "BEncoding metainfo" do
    test "Reads metainfo correctly from test files" do
      let torrentPath = "priv/torrents/archlinux-2023.03.01-x86_64.iso.torrent"
      { announce } <- liftEffect $ expectRight <$> MetaInfo.parseTorrentFile torrentPath
      assertEqual announce archLinuxAnnounceList

expectRight :: forall l r. Either l r -> r
expectRight (Right x) = x
expectRight (Left _) = UnsafePartial.unsafeCrashWith "Expected Right, got Left"

archLinuxAnnounceList :: AnnounceData
archLinuxAnnounceList =
  [ "https://mirror.aarnet.edu.au/pub/archlinux/iso/2023.03.01/"
  , "https://mirrors.rit.edu/archlinux/iso/2023.03.01/"
  , "https://ftp.ek-cer.hu/pub/mirrors/ftp.archlinux.org/iso/2023.03.01/"
  , "https://ftp.heanet.ie/mirrors/ftp.archlinux.org/iso/2023.03.01/"
  , "https://mirror.puzzle.ch/archlinux/iso/2023.03.01/"
  , "https://mirror.csclub.uwaterloo.ca/archlinux/iso/2023.03.01/"
  , "https://mirror.umd.edu/archlinux/iso/2023.03.01/"
  , "https://mirror.archlinux.no/iso/2023.03.01/"
  , "https://mirror.isoc.org.il/pub/archlinux/iso/2023.03.01/"
  , "https://ftp.osuosl.org/pub/archlinux/iso/2023.03.01/"
  , "https://mirror.yandex.ru/archlinux/iso/2023.03.01/"
  , "https://ftp.spline.inf.fu-berlin.de/mirrors/archlinux/iso/2023.03.01/"
  , "https://mirror.selfnet.de/archlinux/iso/2023.03.01/"
  , "https://mirrors.lug.mtu.edu/archlinux/iso/2023.03.01/"
  , "https://archlinux.nautile.nc/archlinux/iso/2023.03.01/"
  , "https://mirrors.kernel.org/archlinux/iso/2023.03.01/"
  , "https://ftp.rnl.tecnico.ulisboa.pt/pub/archlinux/iso/2023.03.01/"
  , "https://mirrors.dotsrc.org/archlinux/iso/2023.03.01/"
  , "https://ftp.jaist.ac.jp/pub/Linux/ArchLinux/iso/2023.03.01/"
  , "https://ftp.halifax.rwth-aachen.de/archlinux/iso/2023.03.01/"
  , "https://archlinux.cs.nycu.edu.tw/iso/2023.03.01/"
  , "https://mirrors.rutgers.edu/archlinux/iso/2023.03.01/"
  , "https://mirrors.nix.org.ua/linux/archlinux/iso/2023.03.01/"
  , "https://mirrors.ustc.edu.cn/archlinux/iso/2023.03.01/"
  , "https://ftp.lysator.liu.se/pub/archlinux/iso/2023.03.01/"
  , "https://mirror.ams1.nl.leaseweb.net/archlinux/iso/2023.03.01/"
  , "https://mirror.dal10.us.leaseweb.net/archlinux/iso/2023.03.01/"
  , "https://mirror.fra10.de.leaseweb.net/archlinux/iso/2023.03.01/"
  , "https://mirror.mia11.us.leaseweb.net/archlinux/iso/2023.03.01/"
  , "https://mirror.sfo12.us.leaseweb.net/archlinux/iso/2023.03.01/"
  , "https://mirror.wdc1.us.leaseweb.net/archlinux/iso/2023.03.01/"
  , "https://mirrors.n-ix.net/archlinux/iso/2023.03.01/"
  , "https://mirror.dkm.cz/archlinux/iso/2023.03.01/"
  , "https://mirror.lnx.sk/pub/linux/archlinux/iso/2023.03.01/"
  , "https://mirror.ps.kz/archlinux/iso/2023.03.01/"
  , "https://mirror.bytemark.co.uk/archlinux/iso/2023.03.01/"
  , "https://mirror.rol.ru/archlinux/iso/2023.03.01/"
  , "https://mirror.i3d.net/pub/archlinux/iso/2023.03.01/"
  , "https://mirrors.tuna.tsinghua.edu.cn/archlinux/iso/2023.03.01/"
  , "https://mirrors.neusoft.edu.cn/archlinux/iso/2023.03.01/"
  , "https://www.mirrorservice.org/sites/ftp.archlinux.org/iso/2023.03.01/"
  , "https://mirror.netcologne.de/archlinux/iso/2023.03.01/"
  , "https://archlinux.vi-di.fr/iso/2023.03.01/"
  , "https://mirror.system.is/arch/iso/2023.03.01/"
  , "https://dfw.mirror.rackspace.com/archlinux/iso/2023.03.01/"
  , "https://hkg.mirror.rackspace.com/archlinux/iso/2023.03.01/"
  , "https://iad.mirror.rackspace.com/archlinux/iso/2023.03.01/"
  , "https://lon.mirror.rackspace.com/archlinux/iso/2023.03.01/"
  , "https://mirror.rackspace.com/archlinux/iso/2023.03.01/"
  , "https://ord.mirror.rackspace.com/archlinux/iso/2023.03.01/"
  , "https://syd.mirror.rackspace.com/archlinux/iso/2023.03.01/"
  , "https://arch.mirror.constant.com/iso/2023.03.01/"
  , "https://mirror.premi.st/archlinux/iso/2023.03.01/"
  , "https://download.nus.edu.sg/mirror/archlinux/iso/2023.03.01/"
  , "https://arch.nimukaito.net/iso/2023.03.01/"
  , "https://mirror.neuf.no/archlinux/iso/2023.03.01/"
  , "https://mirror.gnomus.de/iso/2023.03.01/"
  , "https://ftp.fau.de/archlinux/iso/2023.03.01/"
  , "https://gluttony.sin.cvut.cz/arch/iso/2023.03.01/"
  , "https://mirrors.nic.cz/archlinux/iso/2023.03.01/"
  , "https://mirror.one.com/archlinux/iso/2023.03.01/"
  , "https://mirror.t-home.mk/archlinux/iso/2023.03.01/"
  , "https://ftp.yzu.edu.tw/Linux/archlinux/iso/2023.03.01/"
  , "https://mirror.metalgamer.eu/archlinux/iso/2023.03.01/"
  , "https://mirrors.niyawe.de/archlinux/iso/2023.03.01/"
  , "https://mirrors.atviras.lt/archlinux/iso/2023.03.01/"
  , "https://arch.yourlabs.org/iso/2023.03.01/"
  , "https://arch.midov.pl/arch/iso/2023.03.01/"
  , "https://arch.mirror.zachlge.org/iso/2023.03.01/"
  , "https://archlinux.koyanet.lv/archlinux/iso/2023.03.01/"
  , "https://ftp.myrveln.se/pub/linux/archlinux/iso/2023.03.01/"
  , "https://mirror.telepoint.bg/archlinux/iso/2023.03.01/"
  , "https://archlinux.mailtunnel.eu/iso/2023.03.01/"
  , "https://mirrors.cqu.edu.cn/archlinux/iso/2023.03.01/"
  , "https://archlinux.mirror.digitalpacific.com.au/iso/2023.03.01/"
  , "https://mirror.f4st.host/archlinux/iso/2023.03.01/"
  , "https://mirrors.ocf.berkeley.edu/archlinux/iso/2023.03.01/"
  , "https://ftp.acc.umu.se/mirror/archlinux/iso/2023.03.01/"
  , "https://mirror.hackingand.coffee/arch/iso/2023.03.01/"
  , "https://mirrors.uni-plovdiv.net/archlinux/iso/2023.03.01/"
  , "https://mirror.pseudoform.org/iso/2023.03.01/"
  , "https://mirror.lty.me/archlinux/iso/2023.03.01/"
  , "https://arch.jensgutermuth.de/iso/2023.03.01/"
  , "https://pkg.adfinis.com/archlinux/iso/2023.03.01/"
  , "https://ftp.sh.cvut.cz/arch/iso/2023.03.01/"
  , "https://arch-mirror.wtako.net/iso/2023.03.01/"
  , "https://muug.ca/mirror/archlinux/iso/2023.03.01/"
  , "https://mirror.0x.sg/archlinux/iso/2023.03.01/"
  , "https://mirror.wormhole.eu/archlinux/iso/2023.03.01/"
  , "https://mirror.kaminski.io/archlinux/iso/2023.03.01/"
  , "https://mirror.ubrco.de/archlinux/iso/2023.03.01/"
  , "https://archlinux.ip-connect.vn.ua/iso/2023.03.01/"
  , "https://mirrors.xjtu.edu.cn/archlinux/iso/2023.03.01/"
  , "https://arlm.tyzoid.com/iso/2023.03.01/"
  , "https://archlinux.thaller.ws/iso/2023.03.01/"
  , "https://archimonde.ts.si/archlinux/iso/2023.03.01/"
  , "https://www.ratenzahlung.de/mirror/archlinux/iso/2023.03.01/"
  , "https://mirror.smith.geek.nz/archlinux/iso/2023.03.01/"
  , "https://archlinux.mirror.wearetriple.com/iso/2023.03.01/"
  , "https://mirror.kku.ac.th/archlinux/iso/2023.03.01/"
  , "https://mirror.osbeck.com/archlinux/iso/2023.03.01/"
  , "https://mirrors.pidginhost.com/arch/iso/2023.03.01/"
  , "https://glua.ua.pt/pub/archlinux/iso/2023.03.01/"
  , "https://ftp.lanet.kr/pub/archlinux/iso/2023.03.01/"
  , "https://mirrors.celianvdb.fr/archlinux/iso/2023.03.01/"
  , "https://arch.mirror.square-r00t.net/iso/2023.03.01/"
  , "https://mirror.xtom.com.hk/archlinux/iso/2023.03.01/"
  , "https://mirror.truenetwork.ru/archlinux/iso/2023.03.01/"
  , "https://mirrors.nxthost.com/archlinux/iso/2023.03.01/"
  , "https://mirror-hk.koddos.net/archlinux/iso/2023.03.01/"
  , "https://mirror.koddos.net/archlinux/iso/2023.03.01/"
  , "https://ftp.wrz.de/pub/archlinux/iso/2023.03.01/"
  , "https://mirror.thekinrar.fr/archlinux/iso/2023.03.01/"
  , "https://mirror.neostrada.nl/archlinux/iso/2023.03.01/"
  , "https://mirrors.ukfast.co.uk/sites/archlinux.org/iso/2023.03.01/"
  , "https://archlinux.mivzakim.net/iso/2023.03.01/"
  , "https://mirror.srv.fail/archlinux/iso/2023.03.01/"
  , "https://mirror.reisenbauer.ee/archlinux/iso/2023.03.01/"
  , "https://packages.oth-regensburg.de/archlinux/iso/2023.03.01/"
  , "https://mirror.orbit-os.com/archlinux/iso/2023.03.01/"
  , "https://mirror.stephen304.com/archlinux/iso/2023.03.01/"
  , "https://mirrors.sjtug.sjtu.edu.cn/archlinux/iso/2023.03.01/"
  , "https://mirrors.cat.net/archlinux/iso/2023.03.01/"
  , "https://mirror.ufscar.br/archlinux/iso/2023.03.01/"
  , "https://mirror.oldsql.cc/archlinux/iso/2023.03.01/"
  , "https://arch.mirrors.lavatech.top/iso/2023.03.01/"
  , "https://mirror.checkdomain.de/archlinux/iso/2023.03.01/"
  , "https://mirrors.xtom.com/archlinux/iso/2023.03.01/"
  , "https://dist-mirror.fem.tu-ilmenau.de/archlinux/iso/2023.03.01/"
  , "https://mirror.fsmg.org.nz/archlinux/iso/2023.03.01/"
  , "https://archlinux.grena.ge/iso/2023.03.01/"
  , "https://archlinux.mirror.pcextreme.nl/iso/2023.03.01/"
  , "https://mirror.cyberbits.eu/archlinux/iso/2023.03.01/"
  , "https://repo.ialab.dsu.edu/archlinux/iso/2023.03.01/"
  , "https://arch.unixpeople.org/iso/2023.03.01/"
  , "https://archlinux.mirror.liquidtelecom.com/iso/2023.03.01/"
  , "https://nova.quantum-mirror.hu/mirrors/pub/archlinux/iso/2023.03.01/"
  , "https://quantum-mirror.hu/mirrors/pub/archlinux/iso/2023.03.01/"
  , "https://super.quantum-mirror.hu/mirrors/pub/archlinux/iso/2023.03.01/"
  , "https://archlinux.uk.mirror.allworldit.com/archlinux/iso/2023.03.01/"
  , "https://archlinux.za.mirror.allworldit.com/archlinux/iso/2023.03.01/"
  , "https://mirror.librelabucm.org/archlinux/iso/2023.03.01/"
  , "https://mirror-archlinux.webruimtehosting.nl/iso/2023.03.01/"
  , "https://mirror.netweaver.uk/archlinux/iso/2023.03.01/"
  , "https://mirror.wtnet.de/archlinux/iso/2023.03.01/"
  , "https://mirror.mirohost.net/archlinux/iso/2023.03.01/"
  , "https://ftp.harukasan.org/archlinux/iso/2023.03.01/"
  , "https://mirror.ufro.cl/archlinux/iso/2023.03.01/"
  , "https://mirror.aktkn.sg/archlinux/iso/2023.03.01/"
  , "https://mirrors.xtom.nl/archlinux/iso/2023.03.01/"
  , "https://mirror.scd31.com/arch/iso/2023.03.01/"
  , "https://ftp.icm.edu.pl/pub/Linux/dist/archlinux/iso/2023.03.01/"
  , "https://mirror.mikrogravitation.org/archlinux/iso/2023.03.01/"
  , "https://mirror.chaoticum.net/arch/iso/2023.03.01/"
  , "https://iad.mirrors.misaka.one/archlinux/iso/2023.03.01/"
  , "https://mirror.pit.teraswitch.com/archlinux/iso/2023.03.01/"
  , "https://archlinux.mirror.liteserver.nl/iso/2023.03.01/"
  , "https://mirrors.ims.nksc.lt/archlinux/iso/2023.03.01/"
  , "https://mirrors.eric.ovh/arch/iso/2023.03.01/"
  , "https://mirror.arizona.edu/archlinux/iso/2023.03.01/"
  , "https://mirror.cloroformo.org/archlinux/iso/2023.03.01/"
  , "https://mirror2.evolution-host.com/archlinux/iso/2023.03.01/"
  , "https://mirror.redrock.team/archlinux/iso/2023.03.01/"
  , "https://archmirror1.octyl.net/iso/2023.03.01/"
  , "https://mirror.telkomuniversity.ac.id/archlinux/iso/2023.03.01/"
  , "https://mirror.serverion.com/archlinux/iso/2023.03.01/"
  , "https://plug-mirror.rcac.purdue.edu/archlinux/iso/2023.03.01/"
  , "https://mirror.efect.ro/archlinux/iso/2023.03.01/"
  , "https://mirrors.mit.edu/archlinux/iso/2023.03.01/"
  , "https://arch.hu.fo/archlinux/iso/2023.03.01/"
  , "https://mirrors.chroot.ro/archlinux/iso/2023.03.01/"
  , "https://mirrors.melbourne.co.uk/archlinux/iso/2023.03.01/"
  , "https://mirror.tarellia.net/distr/archlinux/iso/2023.03.01/"
  , "https://mirrors.piconets.webwerks.in/archlinux-mirror/iso/2023.03.01/"
  , "https://mirrors.urbanwave.co.za/archlinux/iso/2023.03.01/"
  , "https://mirror.sysa.tech/archlinux/iso/2023.03.01/"
  , "https://mirror.rasanegar.com/archlinux/iso/2023.03.01/"
  , "https://mirror.wuki.li/archlinux/iso/2023.03.01/"
  , "https://ftp.sudhip.com/archlinux/iso/2023.03.01/"
  , "https://mirrors.bfsu.edu.cn/archlinux/iso/2023.03.01/"
  , "https://mirroir.wptheme.fr/archlinux/iso/2023.03.01/"
  , "https://mirror.kumi.systems/archlinux/iso/2023.03.01/"
  , "https://mirrors.slaanesh.org/archlinux/iso/2023.03.01/"
  , "https://mirrors.nju.edu.cn/archlinux/iso/2023.03.01/"
  , "https://mirrors.gethosted.online/archlinux/iso/2023.03.01/"
  , "https://phinau.de/arch/iso/2023.03.01/"
  , "https://mirror.anquan.cl/archlinux/iso/2023.03.01/"
  , "https://mirrors.daan.vodka/archlinux/iso/2023.03.01/"
  , "https://mirror.satis-faction.de/archlinux/iso/2023.03.01/"
  , "https://mirror.gi.co.id/archlinux/iso/2023.03.01/"
  , "https://mirror.papua.go.id/archlinux/iso/2023.03.01/"
  , "https://mirror.lyrahosting.com/archlinux/iso/2023.03.01/"
  , "https://mirror.hodgepodge.dev/archlinux/iso/2023.03.01/"
  , "https://mirror.dogado.de/archlinux/iso/2023.03.01/"
  , "https://mirror.clientvps.com/archlinux/iso/2023.03.01/"
  , "https://zxcvfdsa.com/arch/iso/2023.03.01/"
  , "https://mirror.ihost.md/archlinux/iso/2023.03.01/"
  , "https://pkg.fef.moe/archlinux/iso/2023.03.01/"
  , "https://mirror.ette.biz/archlinux/iso/2023.03.01/"
  , "https://theswissbay.ch/archlinux/iso/2023.03.01/"
  , "https://archmirror.it/repos/iso/2023.03.01/"
  , "https://mirror.anigil.com/archlinux/iso/2023.03.01/"
  , "https://mirrors.hit.edu.cn/archlinux/iso/2023.03.01/"
  , "https://mirror.hoster.kz/archlinux/iso/2023.03.01/"
  , "https://arch.lucassymons.net/iso/2023.03.01/"
  , "https://ftp.agdsn.de/pub/mirrors/archlinux/iso/2023.03.01/"
  , "https://mirror.ava.dev/archlinux/iso/2023.03.01/"
  , "https://mirror.guillaumea.fr/archlinux/iso/2023.03.01/"
  , "https://vpsmurah.jagoanhosting.com/archlinux/iso/2023.03.01/"
  , "https://mirror.arctic.lol/ArchMirror/iso/2023.03.01/"
  , "https://mirror.surf/archlinux/iso/2023.03.01/"
  , "https://mirror.cspacehostings.com/archlinux/iso/2023.03.01/"
  , "https://arch.mcstrugs.org/iso/2023.03.01/"
  , "https://repo.greeklug.gr/data/pub/linux/archlinux/iso/2023.03.01/"
  , "https://arch.jsc.mx/iso/2023.03.01/"
  , "https://mirror.darklinux.uk/archlinux/iso/2023.03.01/"
  , "https://free.nchc.org.tw/arch/iso/2023.03.01/"
  , "https://mirror.juniorjpdj.pl/archlinux/iso/2023.03.01/"
  , "https://archlinux.qontinuum.space/archlinux/iso/2023.03.01/"
  , "https://mirror.nw-sys.ru/archlinux/iso/2023.03.01/"
  , "https://repo.skni.umcs.pl/archlinux/iso/2023.03.01/"
  , "https://mirror.cybersecurity.nmt.edu/archlinux/iso/2023.03.01/"
  , "https://mirror.0xem.ma/arch/iso/2023.03.01/"
  , "https://mirror.cj2.nl/archlinux/iso/2023.03.01/"
  , "https://mirror.hostup.org/archlinux/iso/2023.03.01/"
  , "https://mirrors.xtom.de/archlinux/iso/2023.03.01/"
  , "https://mirrors.xtom.ee/archlinux/iso/2023.03.01/"
  , "https://mirror.cyberbits.asia/archlinux/iso/2023.03.01/"
  , "https://mirror.moson.org/arch/iso/2023.03.01/"
  , "https://mirror.phx1.us.spryservers.net/archlinux/iso/2023.03.01/"
  , "https://arch.yhtez.xyz/iso/2023.03.01/"
  , "https://mirror.2degrees.nz/archlinux/iso/2023.03.01/"
  , "https://mirror.powerfly.ca/archlinux/iso/2023.03.01/"
  , "https://ftp.ludd.ltu.se/mirrors/archlinux/iso/2023.03.01/"
  , "https://mirror.jingk.ai/archlinux/iso/2023.03.01/"
  , "https://mirrors.wsyu.edu.cn/archlinux/iso/2023.03.01/"
  , "https://mirrors.radwebhosting.com/archlinux/iso/2023.03.01/"
  , "https://mirror.theash.xyz/arch/iso/2023.03.01/"
  , "https://mirror.cov.ukservers.com/archlinux/iso/2023.03.01/"
  , "https://archlinux.astra.in.ua/iso/2023.03.01/"
  , "https://ftp.psnc.pl/linux/archlinux/iso/2023.03.01/"
  , "https://mirror.clarkson.edu/archlinux/iso/2023.03.01/"
  , "https://mirror.repository.id/archlinux/iso/2023.03.01/"
  , "https://repo.endpoint.ml/archlinux/iso/2023.03.01/"
  , "https://mirrors.viflcraft.top/archlinux/iso/2023.03.01/"
  , "https://mirror.iusearchbtw.nl/iso/2023.03.01/"
  , "https://mirrors.up.pt/pub/archlinux/iso/2023.03.01/"
  , "https://mirror.bardia.tech/archlinux/iso/2023.03.01/"
  , "https://at.arch.mirror.kescher.at/iso/2023.03.01/"
  , "https://de.arch.mirror.kescher.at/iso/2023.03.01/"
  , "https://archlinux.homeinfo.de/iso/2023.03.01/"
  , "https://archlinux.ourhome.kiwi/iso/2023.03.01/"
  , "https://mirror.flokinet.net/archlinux/iso/2023.03.01/"
  , "https://mirror.ibakerserver.pt/Arch/iso/2023.03.01/"
  , "https://mirror.funami.tech/arch/iso/2023.03.01/"
  , "https://mirrors.aliyun.com/archlinux/iso/2023.03.01/"
  , "https://mirror.safe-con.dk/archlinux/iso/2023.03.01/"
  , "https://mirror.sg.gs/archlinux/iso/2023.03.01/"
  , "https://mirrors.gandi.net/archlinux/iso/2023.03.01/"
  , "https://os.codefionn.eu/archlinux/iso/2023.03.01/"
  , "https://mirrors.janbruckner.de/archlinux/iso/2023.03.01/"
  , "https://mirror.eloteam.tk/archlinux/iso/2023.03.01/"
  , "https://arch.juline.tech/iso/2023.03.01/"
  , "https://mirror.archlinux.tw/ArchLinux/iso/2023.03.01/"
  , "https://archlinux.mirror.net.in/archlinux/iso/2023.03.01/"
  , "https://mirror.alwyzon.net/archlinux/iso/2023.03.01/"
  , "https://lysakermoen.com/Software/Linux/Mirrors/ArchLinux/iso/2023.03.01/"
  , "https://mirror.theo546.fr/archlinux/iso/2023.03.01/"
  , "https://mirror2.sandyriver.net/pub/archlinux/iso/2023.03.01/"
  , "https://mirror.albony.xyz/archlinux/iso/2023.03.01/"
  , "https://mirror.informatik.tu-freiberg.de/arch/iso/2023.03.01/"
  , "https://depo.turkiye.linux.web.tr/archlinux/iso/2023.03.01/"
  , "https://mirrors.wale.id.au/archlinux/iso/2023.03.01/"
  , "https://mirror.tmmworkshop.com/archlinux/iso/2023.03.01/"
  , "https://mirrors.42tm.tech/archlinux/iso/2023.03.01/"
  , "https://mirroir.labhouse.fr/arch/iso/2023.03.01/"
  , "https://mirrors.bloomu.edu/archlinux/iso/2023.03.01/"
  , "https://mirror.tux.si/arch/iso/2023.03.01/"
  , "https://codingflyboy.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://coresite.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://edgeuno-bog2.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://forksystems.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://gsl-syd.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://irltoolkit.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://mirror.fcix.net/archlinux/iso/2023.03.01/"
  , "https://mnvoip.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://nnenix.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://ridgewireless.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://southfront.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://uvermont.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://ziply.mm.fcix.net/archlinux/iso/2023.03.01/"
  , "https://america.mirror.pkgbuild.com/iso/2023.03.01/"
  , "https://asia.mirror.pkgbuild.com/iso/2023.03.01/"
  , "https://europe.mirror.pkgbuild.com/iso/2023.03.01/"
  , "https://geo.mirror.pkgbuild.com/iso/2023.03.01/"
  , "https://seoul.mirror.pkgbuild.com/iso/2023.03.01/"
  , "https://sydney.mirror.pkgbuild.com/iso/2023.03.01/"
  , "https://mirrors.njupt.edu.cn/archlinux/iso/2023.03.01/"
  , "https://in-mirror.garudalinux.org/archlinux/iso/2023.03.01/"
  , "https://mirror.kamtv.ru/archlinux/iso/2023.03.01/"
  , "https://mirrors.shanghaitech.edu.cn/archlinux/iso/2023.03.01/"
  , "https://mirror.sahil.world/archlinux/iso/2023.03.01/"
  , "https://mirror.xenyth.net/archlinux/iso/2023.03.01/"
  , "https://mirrors.vectair.net/archlinux/iso/2023.03.01/"
  , "https://mirrors.nxtgen.com/archlinux-mirror/iso/2023.03.01/"
  , "https://arch.mirror.ivo.st/iso/2023.03.01/"
  , "https://mirror.misakamikoto.network/archlinux/iso/2023.03.01/"
  , "https://fastmirror.pp.ua/archlinux/iso/2023.03.01/"
  , "https://mirror.mangohost.net/archlinux/iso/2023.03.01/"
  , "https://mirror.lebedinets.ru/archlinux/iso/2023.03.01/"
  , "https://arch.kyberorg.fi/iso/2023.03.01/"
  , "https://mirrors.abhy.me/archlinux/iso/2023.03.01/"
  , "https://mirror.zackmyers.io/archlinux/iso/2023.03.01/"
  , "https://mirror.the-repo.org/ArchMirror/iso/2023.03.01/"
  , "https://mirror.cmt.de/archlinux/iso/2023.03.01/"
  , "https://mirror.yal.sl-chat.ru/archlinux/iso/2023.03.01/"
  , "https://arch.sakamoto.pl/iso/2023.03.01/"
  , "https://mirror.saebasol.org/archlinux/iso/2023.03.01/"
  , "https://mirror.metanet.ch/archlinux/iso/2023.03.01/"
  , "https://mirror.jordanrey.me/archlinux/iso/2023.03.01/"
  , "https://mirror.spaceint.fr/archlinux/iso/2023.03.01/"
  , "https://archlinux.interhost.co.il/iso/2023.03.01/"
  , "https://mirrors.eze.sysarmy.com/archlinux/iso/2023.03.01/"
  , "https://mirror.it4i.cz/arch/iso/2023.03.01/"
  , "https://md.mirrors.hacktegic.com/archlinux/iso/2023.03.01/"
  , "https://archmirror.xyz/archlinux/iso/2023.03.01/"
  , "https://mirror.worldhotspot.org/archlinux/iso/2023.03.01/"
  , "https://mirror.stjschools.org/arch/iso/2023.03.01/"
  , "https://mirror.sunred.org/archlinux/iso/2023.03.01/"
  , "https://m.lqy.me/arch/iso/2023.03.01/"
  , "https://repo.slithery.uk/iso/2023.03.01/"
  , "https://mirror.ditatompel.com/archlinux/iso/2023.03.01/"
  , "https://mirror.infernocomms.net/archlinux/iso/2023.03.01/"
  , "https://arch.mirror.winslow.cloud/iso/2023.03.01/"
  , "https://arch.harald.ga/iso/2023.03.01/"
  , "https://archive.archlinux.org/iso/2023.03.01/"
  ] # List.fromFoldable # AnnounceList

