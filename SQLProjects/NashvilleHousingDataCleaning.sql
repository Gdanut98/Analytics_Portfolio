-- Cleaning the data

SELECT *
FROM PortfolioProject..NashvilleHousing


-- Standardize Date Format

ALTER TABLE NashvilleHousing
ALTER COLUMN [SaleDate] date

-- Populate Property Address

SELECT PropertyAddress
FROM PortfolioProject..NashvilleHousing
WHERE PropertyAddress is null

SELECT A.ParcelID, A.PropertyAddress, B.ParcelID, B.PropertyAddress, ISNULL(A.PropertyAddress, B.PropertyAddress)
FROM PortfolioProject..NashvilleHousing A
JOIN PortfolioProject..NashvilleHousing B
	ON A.ParcelID = B.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
WHERE A.PropertyAddress is null

UPDATE A
SET PropertyAddress = ISNULL(A.PropertyAddress, B.PropertyAddress)
FROM PortfolioProject..NashvilleHousing A
JOIN PortfolioProject..NashvilleHousing B
	ON A.ParcelID = B.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
WHERE A.PropertyAddress is null

--Breaking ot Address into Individual Columns

SELECT PropertyAddress
FROM PortfolioProject..NashvilleHousing

SELECT
SUBSTRING(PropertyAddress, 1, charindex(',', PropertyAddress) - 1) AS Address
, SUBSTRING(PropertyAddress, charindex(',', PropertyAddress) + 1,LEN(PropertyAddress)) AS Address

FROM PortfolioProject..NashvilleHousing

ALTER TABLE NashvilleHousing
ADD PropertySplitAddress NVARCHAR(255);

Update NashvilleHousing
SET PropertySplitAddress = SUBSTRING(PropertyAddress, 1, charindex(',', PropertyAddress) - 1)

ALTER TABLE NashvilleHousing
ADD PropertySplitCity NVARCHAR(255);

Update NashvilleHousing
SET PropertySplitCity = SUBSTRING(PropertyAddress, charindex(',', PropertyAddress) + 1,LEN(PropertyAddress))

-- Now Owner Address

SELECT OwnerAddress
FROM PortfolioProject..NashvilleHousing

SELECT
PARSENAME(REPLACE(OwnerAddress, ',', '.'), 3)
,PARSENAME(REPLACE(OwnerAddress, ',', '.'), 2)
,PARSENAME(REPLACE(OwnerAddress, ',', '.'), 1)
FROM PortfolioProject..NashvilleHousing

ALTER TABLE NashvilleHousing
ADD OwnerSpiltAddress Nvarchar(255);

Update NashvilleHousing
SET OwnerSpiltAddress = PARSENAME(REPLACE(OwnerAddress, ',', '.'), 3)

ALTER TABLE NashvilleHousing
ADD OwnerSpiltCity Nvarchar(255);

Update NashvilleHousing
SET OwnerSpiltCity= PARSENAME(REPLACE(OwnerAddress, ',', '.'), 2)

ALTER TABLE NashvilleHousing
ADD OwnerSpiltState Nvarchar(255);

Update NashvilleHousing
SET OwnerSpiltState= PARSENAME(REPLACE(OwnerAddress, ',', '.'), 1)

-- Change Y and N to Yes and No in "Sold as Vacant" field
SELECT DISTINCT(SoldAsVacant), COUNT(SoldAsVacant)
FROM PortfolioProject..NashvilleHousing
GROUP BY SoldAsVacant
ORDER BY 2

SELECT SoldAsVacant
, CASE When SoldAsVacant = 'Y' Then 'Yes'
		When SoldAsVacant = 'N' then 'No'
		ELSE SoldAsVacant
		END
FROM PortfolioProject..NashvilleHousing

Update NashvilleHousing
SET SoldAsVacant = CASE When SoldAsVacant = 'Y' Then 'Yes'
		When SoldAsVacant = 'N' then 'No'
		ELSE SoldAsVacant
		END


-- Remove Dublicates 

WITH RowNumCTE AS (

SELECT *,
	ROW_NUMBER() OVER (
	Partition BY ParcelID,
				SalePrice,
				PropertyAddress,
				SaleDate,
				LegalReference
				ORDER BY UniqueID
				) row_num


FROM PortfolioProject..NashvilleHousing
-- ORDER BY ParcelID
)
DELETE
FROM RowNumCTE
Where row_num > 1


-- DELETE Unued Columns

Select *
FROM PortfolioProject..NashvilleHousing

ALTER TABLE PortfolioProject..NashvilleHousing
DROP COLUMN OwnerAddress, TaxDistrict, PropertyAddress

ALTER TABLE PortfolioProject..NashvilleHousing
DROP COLUMN SaleDate