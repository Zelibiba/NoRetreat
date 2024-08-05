using Avalonia;
using Avalonia.Controls;
using Avalonia.Input;

namespace HexGameControls
{
    public partial class HexItem : UserControl
    {
        public HexItem()
        {
            InitializeComponent();
        }

        public static readonly StyledProperty<double> DiagonalProperty =
            AvaloniaProperty.Register<HexItem, double>(nameof(Diagonal));
        public static readonly StyledProperty<double> BackGroundOpacityProperty =
            AvaloniaProperty.Register<HexItem, double>(nameof(BackGroundOpacity), defaultValue: 0.0);

        public double Diagonal
        {
            get => GetValue(DiagonalProperty);
            set => SetValue(DiagonalProperty, value);
        }
        public double BackGroundOpacity
        {
            get => GetValue(BackGroundOpacityProperty);
            set => SetValue(BackGroundOpacityProperty, value);
        }

        protected override void OnPropertyChanged(AvaloniaPropertyChangedEventArgs change)
        {
            base.OnPropertyChanged(change);

            if (change.Property == DiagonalProperty)
            {
                double size = Diagonal;
                Height = 2 * size;
                Width = 1.73205080756 * size;
            }
        }
    }
}
