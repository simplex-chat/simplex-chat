// SPDX-License-Identifier: MIT
pragma solidity ^0.8.27;

import "@openzeppelin/contracts@5.4.0/access/Ownable.sol";
import "@openzeppelin/contracts@5.4.0/utils/Pausable.sol";
import "./NFTNumbered.sol";

contract NFTMinter is Ownable, Pausable {
    NFTNumbered public nft;
    uint public mintEndTime;
    uint public mintCount;

    constructor(address _nftAddress, uint _mintEndTime, bool _paused) Ownable(msg.sender) {
        require(_nftAddress != address(0), "NFT contract address is 0");
        require(_mintEndTime > block.timestamp, "Mint end time is in the past");
        nft = NFTNumbered(_nftAddress);
        mintEndTime = _mintEndTime;
        if (_paused) _pause();
    }

    event NFTUpdated(address indexed newNFT);
    event MintEndUpdated(uint256 newTime);

    /// @notice Allows anyone to mint NFT for free (gas only). Any mint restrictions other than not paused or mint time must be in NFT contract
    function mint() external whenNotPaused {
        require(block.timestamp < mintEndTime, "Minting ended");
        nft.mint(msg.sender);
        mintCount++;
    }

    /// @notice Update the target NFT contract.
    /// @param newNFT The new NFT contract address.
    function setNFT(address newNFT) external onlyOwner {
        require(newNFT != address(0), "NFT contract address is 0");
        nft = NFTNumbered(newNFT);
        emit NFTUpdated(newNFT);
    }

    /// @notice Update the mint end timestamp.
    /// @param newTime The new Unix timestamp when minting ends.
    function setMintUntil(uint256 newTime) external onlyOwner {
        require(newTime > block.timestamp, "Mint end time is in the past");
        mintEndTime = newTime;
        emit MintEndUpdated(newTime);
    }

    /// @notice Pause minting.
    function pause() external onlyOwner {
        _pause();
    }

    /// @notice Unpause minting.
    function unpause() external onlyOwner {
        _unpause();
    }

    /// @notice Withdraw any accidental ETH (safety).
    function withdraw() external onlyOwner {
        payable(owner()).transfer(address(this).balance);
    }
}
