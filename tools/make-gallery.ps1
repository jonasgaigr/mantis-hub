# ---------------------------------------------------------------------------
# make-gallery.ps1 -- build the web-sized photographs the index page shows.
#
# index.qmd renders with embed-resources: true, so every photograph in the
# gallery is base64 inside index.html and every byte is served on first paint.
# The originals in images/ are 6000 px camera and phone files running to 10 MB
# each; the gallery shows them in a grid three across. This writes 1100 px
# derivatives into images/gallery/ -- enough for a 2x display at that size,
# about 900 KB for the set.
#
# Run it after adding or replacing a source photograph, then re-render:
#   pwsh tools/make-gallery.ps1
#   quarto render index.qmd
#
# System.Drawing rather than ImageMagick: no dependency to install, and the
# only operations needed are a bicubic downscale and a JPEG encode. Note that
# it ignores EXIF orientation -- if a newly added phone photo comes out
# rotated, rotate the original before running this.
# ---------------------------------------------------------------------------

Add-Type -AssemblyName System.Drawing

function Write-Derivative {
    param(
        [Parameter(Mandatory)][string]$In,
        [Parameter(Mandatory)][string]$Out,
        [int]$MaxWidth = 1100,
        [int]$Quality = 78
    )

    $src = [System.Drawing.Image]::FromFile($In)
    try {
        $w = $src.Width
        $h = $src.Height
        if ($w -gt $MaxWidth) {
            $nw = $MaxWidth
            $nh = [int][Math]::Round($h * $MaxWidth / $w)
        } else {
            $nw = $w
            $nh = $h
        }

        $bmp = New-Object System.Drawing.Bitmap($nw, $nh)
        $g = [System.Drawing.Graphics]::FromImage($bmp)
        $g.InterpolationMode   = [System.Drawing.Drawing2D.InterpolationMode]::HighQualityBicubic
        $g.SmoothingMode       = [System.Drawing.Drawing2D.SmoothingMode]::HighQuality
        $g.PixelOffsetMode     = [System.Drawing.Drawing2D.PixelOffsetMode]::HighQuality
        $g.CompositingQuality  = [System.Drawing.Drawing2D.CompositingQuality]::HighQuality
        # PNG sources may carry alpha; JPEG has none, so flatten onto white
        # rather than letting transparent pixels encode as black.
        $g.Clear([System.Drawing.Color]::White)
        $g.DrawImage($src, 0, 0, $nw, $nh)
        $g.Dispose()

        $codec = [System.Drawing.Imaging.ImageCodecInfo]::GetImageEncoders() |
                 Where-Object { $_.MimeType -eq 'image/jpeg' }
        $ep = New-Object System.Drawing.Imaging.EncoderParameters(1)
        $ep.Param[0] = New-Object System.Drawing.Imaging.EncoderParameter(
            [System.Drawing.Imaging.Encoder]::Quality, [int]$Quality)
        $bmp.Save($Out, $codec, $ep)
        $bmp.Dispose()

        Write-Output ("{0}: {1}x{2} -> {3}x{4}, {5} KB" -f `
            (Split-Path $Out -Leaf), $w, $h, $nw, $nh,
            [int]((Get-Item $Out).Length / 1KB))
    } finally {
        $src.Dispose()
    }
}

$root = Split-Path $PSScriptRoot -Parent
$src  = Join-Path $root 'images'
$dst  = Join-Path $src  'gallery'
if (-not (Test-Path $dst)) { New-Item -ItemType Directory -Path $dst | Out-Null }

# Source file, gallery name, max width, JPEG quality. The order here is the
# order index.qmd shows them in: sensor, what it records, sensor, sensor,
# detail, verification.
$set = @(
    @{ In = 'ami_system_PSX_20251121_121100.jpg'; Out = 'ami-trap-grassland.jpg'; W = 1100; Q = 76 },
    @{ In = '20250703024959-snapshot.jpg';        Out = 'ami-sheet-night.jpg';    W = 1100; Q = 78 },
    @{ In = 'nahravace_PSX_20251121_121348.jpg';  Out = 'acoustic-recorders.jpg'; W = 1100; Q = 78 },
    @{ In = 'Obr_2_CamAlien_Vojik.JPG';           Out = 'camalien-vehicle.jpg';   W = 1100; Q = 78 },
    @{ In = 'kamera_web.jpg';                     Out = 'camalien-camera.jpg';    W = 1100; Q = 80 },
    # The annotation screenshot is a user interface: a little more width and a
    # little less compression, so the labels stay readable.
    @{ In = 'abms_ai_interface.png';              Out = 'ai-verification-ui.jpg'; W = 1400; Q = 84 }
)

foreach ($item in $set) {
    $in = Join-Path $src $item.In
    if (-not (Test-Path $in)) {
        Write-Warning ("missing source: images/{0}" -f $item.In)
        continue
    }
    Write-Derivative -In $in -Out (Join-Path $dst $item.Out) -MaxWidth $item.W -Quality $item.Q
}

Write-Output ("total: {0:N0} KB" -f `
    ((Get-ChildItem $dst | Measure-Object -Property Length -Sum).Sum / 1KB))
