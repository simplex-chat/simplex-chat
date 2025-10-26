// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.4.22 <0.9.0;

// This import is automatically injected by Remix
import "remix_tests.sol";

// This import is required to use custom transaction context
// Although it may fail compilation in 'Solidity Compiler' plugin
// But it will work fine in 'Solidity Unit Testing' plugin
import "remix_accounts.sol";
import "../contracts/NFTMinter.sol";
import "../contracts/NFTNumbered.sol";

// File name has to end with '_test.sol', this file can contain more than one testSuite contracts
contract NFTMinterTest {
    NFTNumbered s;
    NFTMinter m;
    address public owner = address(this);

    function beforeAll() public {
        s = new NFTNumbered(
            "SimpleX NFT: SMPX testnet access",
            "SIMPLEXNFT",
            "https://ipfs.io/ipfs/abcd"
        );
        m = new NFTMinter(address(s));
    }

    function testCreateMinter() public {
        Assert.equal(address(m.nft()), address(s), "bad nft contract");
        Assert.equal(m.mintEndTime(), 0, "bad time");
        Assert.equal(m.owner(), owner, "bad owner");
    }

    function testMinting() public {
        try m.mint() {
            Assert.ok(false, "expected revert");
        } catch Error(string memory reason) {
            Assert.equal(reason, "Minting ended", "bad reason");
        } catch (bytes memory) {
            Assert.ok(false, "unexpected error");
        }
        m.setMintUntil(block.timestamp + 86400);
    }
}
