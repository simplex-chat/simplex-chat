// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.7.0 <0.9.0;
import "remix_tests.sol";
import "../contracts/NFTNumbered.sol";

contract NFTNumberedTest {
    NFTNumbered s;
    function beforeAll () public {
        s = new NFTNumbered(
            "SimpleX Network Token discount NFT",
            "SIMPLEX20",
            "Additional 20% discount for utility token purchase within limited time from launch in 2026.",
            "ipfs:/bafkreiengtftfovffaon2f6vd73vvvmqonuwfhow6fp7v3tnoexwaf4kfe",
            '[{"display_type": "boost_percentage", "trait_type": "Discount increase", "value": 20}]'
        );
    }

    function testTokenNameAndSymbol () public {
        Assert.equal(s.name(), "SimpleX Network Token discount NFT", "token name did not match");
        Assert.equal(s.symbol(), "SIMPLEX20", "token symbol did not match");
    }
}
