// SPDX-License-Identifier: MIT
pragma solidity ^0.8.27;

import "@openzeppelin/contracts@5.4.0/access/Ownable.sol";
import "@openzeppelin/contracts@5.4.0/utils/Pausable.sol";
import "./NFTNumbered.sol";

contract NFTMinter is Ownable, Pausable {
    NFTNumbered public nft;
    uint public mintStartTime;
    uint public mintEndTime;
    uint public mintCount;

    /// @param _mintEndTime Unix timestamp when minting ends, 0 to mint without time limit.
    constructor(address _nftAddress, uint _mintStartTime, uint _mintEndTime, bool _paused) Ownable(msg.sender) {
        _setNFT(_nftAddress);
        require(_mintEndTime == 0 || _mintEndTime > _mintStartTime, "Mint end time is before start time");
        require(_mintEndTime == 0 || _mintEndTime > block.timestamp, "Mint end time is in the past");
        mintStartTime = _mintStartTime;
        mintEndTime = _mintEndTime;
        if (_paused) _pause();
    }

    event NFTUpdated(address indexed newNFT);
    event Minted(address indexed to, uint count);
    event MintStartUpdated(uint newTime);
    event MintEndUpdated(uint newTime);
    event MintCountReset(uint oldCount);

    /// @notice Allows anyone to mint NFT for free (gas only). Any mint restrictions other than not paused or mint time must be in NFT contract
    function mint() external whenNotPaused {
        require(mintEndTime == 0 || mintEndTime > block.timestamp, "Minting ended");
        require(mintStartTime <= block.timestamp, "Minting not started");
        nft.mint(msg.sender);
        mintCount++;
        emit Minted(msg.sender, mintCount);
    }

    /// @notice Update the target NFT contract.
    /// @param newNFT The new NFT contract address.
    function setNFT(address newNFT) external onlyOwner {
        _setNFT(newNFT);
        emit NFTUpdated(newNFT);
    }

    function _setNFT(address _nft) internal {
        require(_nft != address(0), "NFT contract address is 0");
        uint codeSize;
        assembly { codeSize := extcodesize(_nft) }
        require(codeSize > 0, "Not a contract address");
        nft = NFTNumbered(_nft);
    }

    /// @notice Reset mint counter.
    function resetMintCount() external onlyOwner {
        uint old = mintCount;
        mintCount = 0;
        emit MintCountReset(old);
    }

    /// @notice Update the mint start timestamp.
    /// @param newTime The new Unix timestamp when minting ends.
    function setMintStartTime(uint256 newTime) external onlyOwner {
        require(mintEndTime == 0 || newTime < mintEndTime, "Mint end time is before start time");
        mintStartTime = newTime;
        emit MintStartUpdated(newTime);
    }

    /// @notice Update the mint end timestamp.
    /// @param newTime The new Unix timestamp when minting ends, 0 to mint without time limit.
    function setMintEndTime(uint256 newTime) external onlyOwner {
        require(newTime == 0 || newTime > mintStartTime, "Mint end time is before start time");
        require(newTime == 0 || newTime > block.timestamp, "Mint end time is in the past");
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

    /// @notice Withdraw any accidental ETH.
    function withdraw() external onlyOwner {
        payable(owner()).transfer(address(this).balance);
    }
}
