// SPDX-License-Identifier: MIT
pragma solidity ^0.8.27;

import "@openzeppelin/contracts@5.4.0/access/Ownable.sol";
import "./NFTNumbered.sol";

contract NFTMinter is Ownable {
    NFTNumbered public nft;
    uint public mintEndTime;

    constructor(address _nftAddress) Ownable(msg.sender) {
        nft = NFTNumbered(_nftAddress);
    }

    event NFTUpdated(address indexed newNFT);
    event MintEndUpdated(uint256 newTime);

    /// @notice Allows anyone to mint one NFT for free (gas only), if eligible.
    function mint() external {
        require(block.timestamp < mintEndTime, "Minting ended");
        nft.safeMint(msg.sender);
    }

    /// @notice Update the target NFT contract.
    /// @param newNFT The new NFT contract address.
    function setNFT(address newNFT) external onlyOwner {
        nft = NFTNumbered(newNFT);
        emit NFTUpdated(newNFT);
    }

    /// @notice Update the mint end timestamp.
    /// @param newTime The new Unix timestamp when minting ends.
    function setMintUntil(uint256 newTime) external onlyOwner {
        mintEndTime = newTime;
        emit MintEndUpdated(newTime);
    }

    /// @notice Withdraw any accidental ETH (safety).
    function withdraw() external onlyOwner {
        payable(owner()).transfer(address(this).balance);
    }
}
