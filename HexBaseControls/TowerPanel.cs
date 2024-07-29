using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.Presenters;
using Avalonia.Input;
using Avalonia.Interactivity;
using Avalonia.Media.Transformation;
using Avalonia.Reactive;
using Avalonia.VisualTree;
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace HexGameControls
{
    /// <summary>
    /// Панель для выстраивания контролов в стопку.
    /// </summary>
    public class TowerPanel : Panel
    {
        public TowerPanel() 
        {
            AffectsMeasure<TowerPanel>(DeltaPaddingProperty);
            AffectsMeasure<TowerPanel>(ExpandFactorProperty);
            AttachedToVisualTree += OnAttachedToVisualTree;
            DetachedFromVisualTree += OnDetachedFromVisualTree;
        }

        #region Attached/Detached To Visual Tree
        private void OnAttachedToVisualTree(object? sender, VisualTreeAttachmentEventArgs e)
        {
            DoubleTapped += OnDoubleTapped;
        }
        private void OnDetachedFromVisualTree(object? sender, VisualTreeAttachmentEventArgs e)
        {
            DoubleTapped -= OnDoubleTapped;
        }
        #endregion

        // Двойной клик для раскрытия стопки.
        private void OnDoubleTapped(object? sender, TappedEventArgs e)
        {
            e.Handled = true;
            IsExpanded = !IsExpanded;
            ArrangeOverride(DesiredSize);
        }

        #region DeltaPaddingProperty
        /// <summary>
        /// Размер, на который сдигается каждый следующий контрол.
        /// </summary>
        public Size DeltaPadding
        {
            get => GetValue(DeltaPaddingProperty);
            set => SetValue(DeltaPaddingProperty, value);
        }
        /// <summary>
        /// Свойство для <see cref="DeltaPadding"/>.
        /// </summary>
        public static readonly StyledProperty<Size> DeltaPaddingProperty =
            AvaloniaProperty.Register<TowerPanel, Size>(nameof(DeltaPadding));
        #endregion

        #region ExpandFactorProperty
        /// <summary>
        /// Множитель, используемый для раскрытия стопки.
        /// </summary>
        public double ExpandFactor
        {
            get => GetValue(ExpandFactorProperty);
            set => SetValue(ExpandFactorProperty, value);
        }
        /// <summary>
        /// Свойство для <see cref="ExpandFactor"/>.
        /// </summary>
        public static readonly StyledProperty<double> ExpandFactorProperty =
            AvaloniaProperty.Register<TowerPanel, double>(nameof(ExpandFactor), 1.0);
        #endregion

        #region IsExpandedProperty
        bool _isExpanded = false;
        /// <summary>
        /// Флаг, показывающий, раскрыта ли башня.
        /// </summary>
        public bool IsExpanded
        {
            get => _isExpanded;
            private set => SetAndRaise(IsExpandedProperty, ref _isExpanded, value);
        }
        /// <summary>
        /// Свойство для <see cref="IsExpanded"/>.
        /// </summary>
        public static readonly DirectProperty<TowerPanel, bool> IsExpandedProperty =
            AvaloniaProperty.RegisterDirect<TowerPanel, bool>(
                nameof(IsExpanded),
                o => o.IsExpanded,
                (o, v) => o.IsExpanded = v);
        #endregion

        protected override Size MeasureOverride(Size availableSize)
        {
            if (Children.Count == 0)
                return new Size();

            foreach (var child in Children)
                child.Measure(availableSize);
            return Children[0].DesiredSize;
        }
        protected override Size ArrangeOverride(Size finalSize)
        {
            if (Children.Count == 0)
                return finalSize;
            
            Size padding = DeltaPadding;
            double expandFactor = ExpandFactor;
            if (IsExpanded)
            {
                padding = padding.WithHeight(padding.Height * expandFactor)
                                 .WithWidth(padding.Width * expandFactor);
            }

            var firstChild = Children[0];
            var rect = new Rect(0, 0, firstChild.DesiredSize.Width, firstChild.DesiredSize.Height);
            firstChild.Arrange(rect);
            foreach (var child in Children.Skip(1))
            {
                rect = rect.WithX(rect.X + padding.Width);
                rect = rect.WithY(rect.Y + padding.Height);
                child.Arrange(rect);
            }
            return firstChild.DesiredSize;
        }
    }
}