// SPDX-License-Identifier: MIT
pragma solidity ^0.8.27;

import "@openzeppelin/contracts@5.4.0/token/ERC721/extensions/ERC721Enumerable.sol";
import {IERC721} from "@openzeppelin/contracts@5.4.0/token/ERC721/IERC721.sol";
import "@openzeppelin/contracts@5.4.0/access/Ownable.sol";

/// @title NFTNumbered
/// @notice ERC721 contract with sequential variants for immutable metadata for minted tokens, allowing to change metadata for future tokens.
/// @dev Non-upgradeable. Minter address is settable by owner. Global sequential token ID.
contract NFTNumbered is ERC721Enumerable, Ownable {
    address public minter;
    bool public mintingLocked;
    TokenInfo[] private tokenInfos;
    uint public nextTokenId = 1;

    struct TokenInfo {
        uint tokenId;
        string tokenUri;
    }

    constructor(
        string memory _name,
        string memory _symbol,
        string memory _tokenUri
    ) ERC721(_name, _symbol) Ownable(msg.sender) {
        minter = msg.sender;
        mintingLocked = false;
        setNextTokenURI(_tokenUri);
    }

    event MinterUpdated(address indexed newMinter);
    event MintingLocked();
    event NextTokenURI(uint nextTokenId, string nextTokenUri);

    /// @notice Updates the minter address.
    function setMinter(address newMinter) external onlyOwner whenNotLocked {
        minter = newMinter;
        emit MinterUpdated(newMinter);
    }

    /// @notice Permanently disable minting and changes.
    function lockMintingPermanently() external onlyOwner {
        mintingLocked = true;
        emit MintingLocked();
    }

    modifier whenNotLocked() {
        require(!mintingLocked, "Contract permanently locked for changes and minting");
        _;
    }

    /// @notice Sets tokenUri for future mints only.
    function setNextTokenURI(string memory newTokenUri) public onlyOwner whenNotLocked {
        uint len = tokenInfos.length;
        TokenInfo memory newInfo = TokenInfo({tokenId: nextTokenId, tokenUri: newTokenUri});
        if (len == 0 || tokenInfos[len - 1].tokenId < nextTokenId) { // add
            tokenInfos.push(newInfo);
        } else { // replace
            tokenInfos[len - 1] = newInfo;
        }
        emit NextTokenURI(nextTokenId, newTokenUri);
    }

    /// @notice metadata URI of the token that will be minted next
    function nextTokenURI() external view returns (string memory) {
        return tokenInfos[tokenInfos.length - 1].tokenUri;
    }

    /// @notice Mints a new token.
    /// @param to The recipient of the token.
    function mint(address to) external whenNotLocked {
        require(msg.sender == owner() || msg.sender == minter, "Caller must be the owner or minter");
        require(to != address(0), "Cannot mint to zero address");
        uint tokenId = nextTokenId++;
        _safeMint(to, tokenId);
    }

    /// @notice Burn owned token.
    function burn(uint tokenId) external {
        _burn(tokenId);
    }

    /// @notice prohibit approvals
    // address to, uint256 tokenId
    function approve(address, uint256) public virtual override (ERC721, IERC721) {
        revert("Soulbound token: approvals prohibited");
    }

    /// @notice prohibit approvals
    // address operator, bool approved
    function setApprovalForAll(address, bool) public virtual override (ERC721, IERC721) {
        revert("Soulbound token: approvals prohibited");
    }

    /// @notice Limits ownership to 1 token.
    function _update(address to, uint tokenId, address auth) internal virtual override returns (address) {
        require(to == address(0) || balanceOf(to) == 0, "Soulbound token: only 1 per address");
        require(to == address(0) || _ownerOf(tokenId) == address(0), "Soulbound token: transfers prohibited");
        return super._update(to, tokenId, auth);
    }

    /// @notice Returns embedded JSON metadata URI.
    /// @param id Token ID.
    function tokenURI(uint id) public view virtual override returns (string memory) {
        _requireOwned(id);
        uint len = tokenInfos.length;
        for (uint i = len; i > 0; ) {
            unchecked { i--; }
            if (tokenInfos[i].tokenId > id) continue;
            return tokenInfos[i].tokenUri;
        }
        revert("Unknown token ID");
    }

    /// @notice Withdraw any accidental ETH.
    function withdraw() external onlyOwner {
        (bool success, ) = payable(owner()).call{value: address(this).balance}("");
        require(success, "Withdraw failed");
    }
}
