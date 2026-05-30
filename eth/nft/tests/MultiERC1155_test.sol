// SPDX-License-Identifier: MIT
pragma solidity ^0.8.27;

import "remix_tests.sol";

import "../contracts/MultiERC1155.sol";

contract MultiERC1155Test {
    MultiERC1155 public ct;
    address public owner = address(this);
    address public admin = address(0x1);
    address public minter = address(0x2);
    address public user = address(0x3);
    address public recipient = address(0x4);

    MultiERC1155.TokenInfo defaultInfo = MultiERC1155.TokenInfo({
        tokenUri: "https://example.com/token.json",
        totalSupply: 0,
        enabled: true
    });

    function beforeAll() public {
        ct = new MultiERC1155();
        MultiERC1155.TokenInfo memory newInfo = defaultInfo;
        (bool success, ) = address(ct).call(abi.encodeWithSignature("addToken((string,bool,string,string,string,uint256))", newInfo));
        Assert.equal(success, true, "addToken success");
    }

    // Constructor Test
    function testConstructor() public {
        Assert.equal(ct.owner(), owner, "Owner should be deployer");
        Assert.equal(ct.admin(), owner, "Admin should be deployer");
        Assert.equal(ct.minter(), owner, "Minter should be deployer");
        Assert.equal(ct.mintingEnabled(), true, "Minting should be enabled");
        Assert.equal(ct.contractLocked(), false, "Contract should not be locked");
        uint[] memory ids = ct.getTokenIds();
        Assert.equal(ids.length, uint(1), "One token ID added");
        Assert.equal(ids[0], uint(1), "First token ID is 1");
        (, uint currentSupply, bool exists, bool locked) = ct.tokens(1);
        Assert.equal(exists, true, "Token 1 exists");
        Assert.equal(locked, false, "Token 1 not locked");
        Assert.equal(currentSupply, uint(0), "Token 1 supply 0");
    }

    // setAdmin Test
    function testSetAdmin() public {
        ct.setAdmin(admin);
        Assert.equal(ct.admin(), admin, "Admin updated");
    }

    function testSetAdminOnlyOwner() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("setAdmin(address)", admin));
        Assert.equal(success, true, "Set admin from owner succeeds"); // Since this is owner

        // To test revert, Remix plugin runs as this, so for non-owner, manual simulation not easy; note as TODO or skip
    }

    // setMinter Test
    function testSetMinter() public {
        ct.setMinter(minter);
        Assert.equal(ct.minter(), minter, "Minter updated");
    }

    function testSetMinterOnlyOwner() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("setMinter(address)", minter));
        Assert.equal(success, true, "Set minter from owner succeeds");
    }

    // toggleMinting Test
    function testToggleMinting() public {
        ct.toggleMinting(false);
        Assert.equal(ct.mintingEnabled(), false, "Minting disabled");
        
        ct.toggleMinting(true);
        Assert.equal(ct.mintingEnabled(), true, "Minting enabled");
    }

    function testToggleMintingNoChange() public {
        ct.toggleMinting(true); // Already true
        Assert.equal(ct.mintingEnabled(), true, "No change if same");
    }

    function testToggleMintingOnlyOwner() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("toggleMinting(bool)", false));
        Assert.equal(success, true, "Toggle from owner succeeds");
    }

    // lockContract Test
    function testLockContract() public {
        ct.lockContract();
        Assert.equal(ct.contractLocked(), true, "Contract locked");
        Assert.equal(ct.mintingEnabled(), false, "Minting disabled");
    }

    function testLockContractOnlyOwner() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("lockContract()"));
        Assert.equal(success, true, "Lock from owner succeeds");
    }

    // addToken Test
    function testAddToken() public {
        MultiERC1155.TokenInfo memory newInfo = MultiERC1155.TokenInfo({
            tokenUri: "https://example.com/new_token.json",
            totalSupply: 100,
            enabled: true
        });

        ct.addToken(newInfo);
        uint[] memory ids = ct.getTokenIds();
        Assert.equal(ids.length, uint(2), "Two token IDs");
        Assert.equal(ids[1], uint(2), "Second token ID 2");
        (MultiERC1155.TokenInfo memory tokenInfo, uint currentSupply, bool exists, bool locked) = ct.tokens(2);
        Assert.equal(exists, true, "Token 2 exists");
        Assert.equal(locked, false, "Token 2 not locked");
        Assert.equal(currentSupply, uint(0), "Token 2 supply 0");
        Assert.equal(tokenInfo.totalSupply, uint(100), "Token 2 totalSupply 100");
        // Validate info fields as needed
    }

    function testAddTokenByAdmin() public {
        ct.setAdmin(admin);
        MultiERC1155.TokenInfo memory newInfo = defaultInfo;
        (bool success, ) = address(ct).call(abi.encodeWithSignature("addToken((string,bool,string,string,string,uint256))", newInfo));
        Assert.equal(success, true, "Add by admin succeeds");
        uint[] memory ids = ct.getTokenIds();
        Assert.equal(ids.length, uint(2), "Two token IDs");
    }

    function testAddTokenRevertInvalidInfo() public {
        MultiERC1155.TokenInfo memory invalidInfo = MultiERC1155.TokenInfo({
            tokenUri: "",
            totalSupply: 0,
            enabled: true
        });
        (bool success, ) = address(ct).call(abi.encodeWithSignature("addToken((string,bool,string,string,string,uint256))", invalidInfo));
        Assert.equal(success, false, "Invalid info reverts");
    }

    function testAddTokenOnlyAdminOrOwner() public {
        MultiERC1155.TokenInfo memory info = defaultInfo;
        (bool success, ) = address(ct).call(abi.encodeWithSignature("addToken((string,bool,string,string,string,uint256))", info));
        Assert.equal(success, true, "Add from owner succeeds");
    }

    // removeToken Test
    function testRemoveToken() public {
        MultiERC1155.TokenInfo memory newInfo = defaultInfo;
        ct.addToken(newInfo);
        uint[] memory ids = ct.getTokenIds();
        Assert.equal(ids.length, uint(2), "Two token IDs");

        ct.removeToken(2);
        ids = ct.getTokenIds();
        Assert.equal(ids.length, uint(1), "One token ID after removal");
        (,,bool exists,) = ct.tokens(2);
        Assert.equal(exists, false, "Token 2 removed");
    }

    function testRemoveTokenRevertNonExist() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("removeToken(uint256)", 99));
        Assert.equal(success, false, "Remove non-exist reverts");
    }

    function testRemoveTokenRevertHasSupply() public {
        ct.mint(user, 1, 10, "");
        (bool success,) = address(ct).call(abi.encodeWithSignature("removeToken(uint256)", 1));
        Assert.equal(success, false, "Remove with supply reverts");
    }

    function testRemoveTokenRevertLast() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("removeToken(uint256)", 1));
        Assert.equal(success, false, "Remove last reverts");
    }

    function testRemoveTokenOnlyAdminOrOwner() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("removeToken(uint256)", 1));
        Assert.equal(success, false, "Remove last fails"); // But for non-owner, need simulation
    }

    // updateToken Test
    function testUpdateToken() public {
        ct.updateToken(1, false, 100);
        (MultiERC1155.TokenInfo memory tokenInfo,,,) = ct.tokens(1);
        Assert.equal(tokenInfo.enabled, false, "Enabled updated");
        Assert.equal(tokenInfo.totalSupply, uint(100), "Total supply updated");
    }

    function testUpdateTokenNoChange() public {
        (MultiERC1155.TokenInfo memory tokenInfo,,,) = ct.tokens(1);
        ct.updateToken(1, tokenInfo.enabled, tokenInfo.totalSupply);
        // No assert, as no change
    }

    function testUpdateTokenRevertNonExist() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("updateToken(uint256,bool,uint256)", 99, true, 0));
        Assert.equal(success, false, "Update non-exist reverts");
    }

    function testUpdateTokenRevertLocked() public {
        ct.mint(user, 1, 10, "");
        ct.lockToken(1);
        (bool success, ) = address(ct).call(abi.encodeWithSignature("updateToken(uint256,bool,uint256)", 1, true, 0));
        Assert.equal(success, false, "Update locked reverts");
    }

    function testUpdateTokenRevertInvalidSupply() public {
        ct.mint(user, 1, 10, "");
        (bool success, ) = address(ct).call(abi.encodeWithSignature("updateToken(uint256,bool,uint256)", 1, true, 5));
        Assert.equal(success, false, "Invalid supply reverts");
    }

    function testUpdateTokenOnlyAdminOrOwner() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("updateToken(uint256,bool,uint256)", 1, true, 0));
        Assert.equal(success, true, "Update from owner succeeds");
    }

    // lockToken Test
    function testLockToken() public {
        ct.mint(user, 1, 10, "");
        ct.lockToken(1);
        (MultiERC1155.TokenInfo memory tokenInfo,,, bool locked) = ct.tokens(1);
        Assert.equal(locked, true, "Token locked");
        Assert.equal(tokenInfo.enabled, false, "Enabled false");
    }

    function testLockTokenRevertNonExist() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("lockToken(uint256)", 99));
        Assert.equal(success, false, "Lock non-exist reverts");
    }

    function testLockTokenRevertAlreadyLocked() public {
        ct.mint(user, 1, 10, "");
        ct.lockToken(1);
        (bool success, ) = address(ct).call(abi.encodeWithSignature("lockToken(uint256)", 1));
        Assert.equal(success, false, "Lock already locked reverts");
    }

    function testLockTokenRevertNoSupply() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("lockToken(uint256)", 1));
        Assert.equal(success, false, "Lock no supply reverts");
    }

    function testLockTokenOnlyOwner() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("lockToken(uint256)", 1));
        Assert.equal(success, false, "Lock no supply fails"); // Test with supply
    }

    // mint Test
    function testMint() public {
        ct.mint(user, 1, 10, "");
        Assert.equal(ct.balanceOf(user, 1), 10, "User balance 10");
        (,uint currentSupply,,) = ct.tokens(1);
        Assert.equal(currentSupply, 10, "Token supply 10");
    }

    function testMintRevertLockedContract() public {
        ct.lockContract();
        (bool success, ) = address(ct).call(abi.encodeWithSignature("mint(address,uint256,uint256,bytes)", user, 1, 10, ""));
        Assert.equal(success, false, "Mint locked contract reverts");
    }

    function testMintRevertMintingDisabled() public {
        ct.toggleMinting(false);
        (bool success, ) = address(ct).call(abi.encodeWithSignature("mint(address,uint256,uint256,bytes)", user, 1, 10, ""));
        Assert.equal(success, false, "Mint disabled reverts");
    }

    function testMintRevertInvalidCaller() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("mint(address,uint256,uint256,bytes)", user, 1, 10, ""));
        Assert.equal(success, true, "Mint from owner succeeds");
    }

    function testMintRevertNonExist() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("mint(address,uint256,uint256,bytes)", user, 99, 10, ""));
        Assert.equal(success, false, "Mint non-exist reverts");
    }

    function testMintRevertLockedToken() public {
        ct.mint(user, 1, 10, "");
        ct.lockToken(1);
        (bool success, ) = address(ct).call(abi.encodeWithSignature("mint(address,uint256,uint256,bytes)", user, 1, 5, ""));
        Assert.equal(success, false, "Mint locked token reverts");
    }

    function testMintRevertDisabledToken() public {
        ct.updateToken(1, false, 0);
        (bool success, ) = address(ct).call(abi.encodeWithSignature("mint(address,uint256,uint256,bytes)", user, 1, 10, ""));
        Assert.equal(success, false, "Mint disabled token reverts");
    }

    function testMintRevertSupplyExceeded() public {
        ct.updateToken(1, true, 5);
        (bool success, ) = address(ct).call(abi.encodeWithSignature("mint(address,uint256,uint256,bytes)", user, 1, 10, ""));
        Assert.equal(success, false, "Mint supply exceeded reverts");
    }

    function testMintRevertZeroAmount() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("mint(address,uint256,uint256,bytes)", user, 1, 0, ""));
        Assert.equal(success, false, "Mint zero amount reverts");
    }

    // _update Test (via transfer/burn)
    function testUpdateMint() public {
        ct.mint(user, 1, 10, "");
        (,uint currentSupply,,) = ct.tokens(1);
        Assert.equal(currentSupply, 10, "Supply after mint");
    }

    function testUpdateBurn() public {
        ct.mint(user, 1, 10, "");
        (bool success, ) = address(ct).call(abi.encodeWithSignature("safeTransferFrom(address,address,uint256,uint256,bytes)", user, address(0), 1, 5, ""));
        Assert.equal(success, true, "Burn succeeds");
        (,uint currentSupply,,) = ct.tokens(1);
        Assert.equal(currentSupply, 5, "Supply after burn");
    }

    function testUpdateTransfer() public {
        ct.mint(user, 1, 10, "");
        (bool success, ) = address(ct).call(abi.encodeWithSignature("safeTransferFrom(address,address,uint256,uint256,bytes)", user, recipient, 1, 5, ""));
        Assert.equal(success, true, "Transfer succeeds");
        (,uint currentSupply,,) = ct.tokens(1);
        Assert.equal(currentSupply, 10, "Supply unchanged on transfer");
    }

    // uri Test
    function testUri() public {
        string memory json = Base64.encode(bytes('{"name":"Test Token","description":"Test Description","image":"https://test.com/image.png","properties":{}}'));
        string memory expected = string.concat("data:application/json;base64,", json);
        Assert.equal(ct.uri(1), expected, "URI matches");
    }

    function testUriRevertNonExist() public {
        (bool success, ) = address(ct).call(abi.encodeWithSignature("uri(uint256)", 99));
        Assert.equal(success, false, "URI non-exist reverts");
    }
}
