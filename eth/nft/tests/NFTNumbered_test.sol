// SPDX-License-Identifier: GPL-3.0

pragma solidity >=0.7.0 <0.9.0;
import "remix_tests.sol";
import "remix_accounts.sol";
import "../contracts/NFTNumbered.sol";

contract NFTNumberedTest {
    NFTNumbered s;
    address public owner = address(this);
    address public user1 = address(0x1);
    address public user2 = address(0x2);
    address public user3 = address(0x3);
    address public user4 = TestsAccounts.getAccount(1);
    address public minter = address(0x5);

    function beforeAll () public {
        s = new NFTNumbered(
            "SimpleX NFT: SMPX testnet access",
            "SIMPLEX2026",
            "https://ipfs.io/ipfs/abcd"
        );
    }

    function testCreateToken () public {
        Assert.equal(s.name(), "SimpleX NFT: SMPX testnet access", "bad name");
        Assert.equal(s.symbol(), "SIMPLEX2026", "bad symbol");
        Assert.equal(s.nextTokenId(), 1, "bad next token ID");
        Assert.equal(s.minter(), s.owner(), "minter different from owner");
        Assert.equal(s.mintingLocked(), false, "minting locked");
        Assert.equal(s.owner(), owner, "bad owner");
    }

    function testTransferToken() public {
        Assert.equal(s.balanceOf(user3), 0, "bad balance");
        s.mint(user4);
        Assert.equal(s.balanceOf(user4), 1, "bad balance");
        /// #sender: account-1
        // try s.safeTransferFrom(user4, user3, 1) {
        //     Assert.ok(false, "expected revert");
        // } catch Error(string memory reason) {
        //     Assert.equal(reason, "Token is soulbound: transfers are prohibited", "bad reason");
        // } catch (bytes memory data) {
        //     Assert.ok(false, "unexpected error 2");
        // }
    }

    function testMinting () public {
        s.mint(user1);
        Assert.equal(s.balanceOf(user1), 1, "bad balance");
        Assert.equal(s.tokenURI(2), "https://ipfs.io/ipfs/abcd", "bad URI");
        try s.mint(user1) {
            Assert.ok(false, "expected revert");
        } catch Error(string memory reason) {
            Assert.equal(reason, "Soulbound token: only 1 per address", "bad reason");
        } catch (bytes memory) {
            Assert.ok(false, "unexpected error");
        }
        Assert.equal(s.nextTokenId(), 3, "bad next token ID");
        s.mint(user2);
        Assert.equal(s.balanceOf(user2), 1, "bad balance");
        Assert.equal(s.tokenURI(3), "https://ipfs.io/ipfs/abcd", "bad URI");
        Assert.equal(s.nextTokenURI(), "https://ipfs.io/ipfs/abcd", "bad URI");
        s.setNextTokenURI("https://ipfs.io/ipfs/efgh");
        Assert.equal(s.nextTokenURI(), "https://ipfs.io/ipfs/efgh", "bad URI");
        Assert.equal(s.balanceOf(user3), 0, "bad balance");
        s.mint(user3);
        Assert.equal(s.balanceOf(user3), 1, "bad balance");
        Assert.equal(s.tokenURI(4), "https://ipfs.io/ipfs/efgh", "bad URI");
    }

    function testMintingLock () public {
        s.lockMintingPermanently();
        try s.mint(user2) {
            Assert.ok(false, "expected revert");
        } catch Error(string memory reason) {
            Assert.equal(reason, "Contract permanently locked for changes and minting", "bad reason");
        } catch (bytes memory) {
            Assert.ok(false, "unexpected error");
        }
    }
}
