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

        public static readonly StyledProperty<double> RadiusProperty =
            AvaloniaProperty.Register<HexItem, double>(nameof(Radius));
        public static readonly StyledProperty<double> BackGroundOpacityProperty =
            AvaloniaProperty.Register<HexItem, double>(nameof(BackGroundOpacity), defaultValue: 1.0);

        public double Radius
        {
            get => GetValue(RadiusProperty);
            set => SetValue(RadiusProperty, value);
        }
        public double BackGroundOpacity
        {
            get => GetValue(BackGroundOpacityProperty);
            set => SetValue(BackGroundOpacityProperty, value);
        }

        protected override void OnPropertyChanged(AvaloniaPropertyChangedEventArgs change)
        {
            base.OnPropertyChanged(change);

            if (change.Property == RadiusProperty)
            {
                double size = Radius;
                Height = 2 * size;
                Width = 1.73205080756 * size;
            }
        }
    }
}
