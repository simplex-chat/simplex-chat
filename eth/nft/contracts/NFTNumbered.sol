// SPDX-License-Identifier: MIT
pragma solidity ^0.8.27;

import "@openzeppelin/contracts@5.4.0/token/ERC721/ERC721.sol";
import "@openzeppelin/contracts@5.4.0/access/Ownable.sol";
import "@openzeppelin/contracts@5.4.0/utils/Base64.sol";
import "@openzeppelin/contracts@5.4.0/utils/Strings.sol";

contract NFTNumbered is ERC721, Ownable {
    address public minter;
    bool public mintingEnabled;
    string public description;
    string public image;
    string public attributes;
    uint private _nextTokenId = 1;

    constructor(
        string memory _name,
        string memory _symbol,
        string memory _description,
        string memory _image,
        string memory _attributes
    ) ERC721(_name, _symbol) Ownable(msg.sender) {
        minter = msg.sender;
        mintingEnabled = true;
        description = _description;
        image = _image;
        attributes = _attributes;
    }

    event MinterUpdated(address indexed newMinter);
    event MintingEnabled(bool newEnabled);

    /// @notice Updates the minter address.
    /// @param newMinter The new minter.
    function setMinter(address newMinter) external onlyOwner {
        minter = newMinter;
        emit MinterUpdated(newMinter);
    }

    /// @notice Enables/disables minting.
    /// @param enabled True/false to enable/disable minting.
    function toggleMinting(bool enabled) external onlyOwner {
        if (mintingEnabled != enabled) {
            mintingEnabled = enabled;
            emit MintingEnabled(enabled);
        }
    }

    /// @notice Mints a new token.
    /// @param to The recipient of the token.
    function safeMint(address to) external {
        require(msg.sender == owner() || msg.sender == minter, "Caller must be the owner or minter");
        require(mintingEnabled, "Minting is disabled");
        uint tokenId = _nextTokenId++;
        _safeMint(to, tokenId);
    }

    function _update(address to, uint tokenId, address auth) internal virtual override returns (address) {
        require(to == address(0) || balanceOf(to) == 0, "Recipient already owns this token, only one token per address is allowed");
        return super._update(to, tokenId, auth);
    }


    /// @notice Returns embedded JSON metadata URI.
    /// @param id Token ID.
    function tokenURI(uint id) public view virtual override returns (string memory) {
        string memory json = Base64.encode(
            bytes(
                string.concat(
                    '{"name":"', name(), ' #', Strings.toString(id),
                    '","description":"', description,
                    '","image":"', image,
                    '","attributes":', attributes,
                    '}'
                )
            )
        );
        return string.concat("data:application/json;base64,", json);
    }

    /// @notice Withdraw any accidental ETH (safety).
    function withdraw() external onlyOwner {
        payable(owner()).transfer(address(this).balance);
    }
}
